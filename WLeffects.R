require(tidyverse)
require(lme4)
require(MCMCglmm)
require(parallel)
require(data.table)
require(anytime)

#################################################################################
# read data file  ###############################################################
#################################################################################

# select data file
esport.data <- WLdatalong
esport.data
esport.data <- esport.data %>% arrange(desc(begin_at))

# EXTRACT DATE AND TIME FROM begin_at
esport.data <- esport.data %>% separate_wider_delim(begin_at, delim = "T", names = c("begin_date", "begin_time"))

# FUNCTION TO Z-SCORE THE DAYS SINCE LAST CONTEST
# add_zDays.onegroup=function(elodata.onegroup){
#   allDays=c(elodata.onegroup$hDays,elodata.onegroup$aDays)
#   mean.days=mean(allDays)
#   sd.days=sd(allDays)
#   
#   elodata.onegroup$zDays.h=(elodata.onegroup$hDays-mean.days)/sd.days
#   elodata.onegroup$zDays.a=(elodata.onegroup$aDays-mean.days)/sd.days
#   
#   return(elodata.onegroup)
# }

get_previous.outcome.predictions=function(model,parameter.indexes){
  n.poutcomes=length(parameter.indexes)
  prediction.dfs=vector(n.poutcomes,mode="list")
  coef.range=seq(from=0,to=1,by=0.01)
  coefs=as.numeric(fixef(model))
  for(i in 1:n.poutcomes){
    param.ests=vector(length(coefs),mode="list")
    param.ests[[1]]=coefs[1]
    for(j in 2:length(param.ests)){
      if(j==parameter.indexes[i]){param.ests[[j]]=coefs[j]*coef.range} else {param.ests[[j]]=coefs[j]*0}
    }
    x.logits=Reduce("+",param.ests)
    x.probs=exp(x.logits)/(1 + exp(x.logits))
    prediction.dfs[[i]]=data.frame(x=coef.range,predicted=x.probs,nth.previous.outcome=rep.int(i,length(coef.range))) #this assumes that nth previous is always sequential from 1
    
  }
  prediction.dfs=do.call(rbind,prediction.dfs)
  prediction.dfs$nth.previous.outcome=as.factor(prediction.dfs$nth.previous.outcome)
  return(prediction.dfs)
}

# TREAT CALENDAR YEAR AS Season
esport.data$Season=as.factor(format(as.Date(esport.data$begin_date, format="%Y-%m-%d"),"%Y"))
esport.data=split(esport.data,f = esport.data$Season)
#esport.data=lapply(esport.data,add_zDays.onegroup)
esport.data=do.call(rbind,esport.data)
esport.data$index=seq(from=1,to=nrow(esport.data),by=1)
esport.data


######################################################################################
# Randomly assign either team as the focal team and the other as the opponent and 
# rearrange the data sheet to reflect this
######################################################################################

#choose either opponent_0.id or opponent_1.id to be the focal team in a particular game
esport.data$assigned.focal=character(nrow(esport.data))
for(i in 1:nrow(esport.data)){
  random=runif(n = 1,min = 0,max = 1)
  if(random==0.5){random=runif(n = 1,min = 0,max = 1)}
  esport.data$assigned.focal[i]=ifelse(random>0.5,"opp0","opp1")
}

######################################################################################
# Deal with currency column
######################################################################################

# separate prize pool amount and currency
esport.data <- separate(esport.data, tournament.prizepool, into = c(
  "tournament.prizepool.amount", "tournament.prizepool.currency"
  ), sep = "\\s", extra = "merge")

# convert prize pool amount column to int datatype
esport.data <- esport.data %>% mutate(across(tournament.prizepool.amount, as.integer))

# identify if separation worked as intended
unique(esport.data$tournament.prizepool.currency)

# saw that some values of United States Dollar had extra white space, so trim is necessary
esport.data$tournament.prizepool.currency <- trimws(esport.data$tournament.prizepool.currency)

# convert currency string to relevant abbreviation
esport.data <- esport.data %>%
  mutate(tournament.prizepool.currencycode = case_when(
    tournament.prizepool.currency == "United States Dollar" ~ "USD",
    tournament.prizepool.currency == "Chinese Yuan" ~ "CNY",
    tournament.prizepool.currency == "Euro" ~ "EUR",
    tournament.prizepool.currency == "Brazilian Real" ~ "BRL",
    tournament.prizepool.currency == "Polish Zloty" ~ "PLN",
    tournament.prizepool.currency == "Indian Rupee" ~ "INR",
    tournament.prizepool.currency == "Swedish Krona" ~ "SEK",
    tournament.prizepool.currency == "British Pound"  ~ "GBP",
    tournament.prizepool.currency == "Kazakhstani Tenge" ~ "KZT",
    tournament.prizepool.currency == "Russian Ruble" ~ "RUB",
    tournament.prizepool.currency == "Argentine Peso" ~ "ARS",
    tournament.prizepool.currency == "Norwegian Krone" ~ "NOK",
    tournament.prizepool.currency == "Danish Krone" ~ "DKK",
    tournament.prizepool.currency == "Czech Koruna" ~ "CZK",
    tournament.prizepool.currency == "Australian Dollar" ~ "AUD",
    tournament.prizepool.currency == "Swiss Franc" ~ "CHF",
    tournament.prizepool.currency == "Turkish Lira" ~ "TRY",
    tournament.prizepool.currency == "Japanese Yen" ~ "JPY",
    tournament.prizepool.currency == "Croatian Kuna"  ~ "HRK",
    tournament.prizepool.currency == "Vietnamese Dong" ~ "VND",
    tournament.prizepool.currency == "Icelandic Krona" ~ "ISK",
    tournament.prizepool.currency == "Qatari Riyal" ~ "QAR",
    tournament.prizepool.currency == "Mongolian Togrog" ~ "MNT",
    tournament.prizepool.currency == "Ukrainian Hryvnia" ~ "UAH",
    tournament.prizepool.currency == "Iranian Rial" ~ "IRR",
    tournament.prizepool.currency == "South African Rand" ~ "ZAR",
    tournament.prizepool.currency == "Serbian Dinar" ~ "RSD",
    tournament.prizepool.currency == "Bulgarian Lev" ~ "BGN",
    TRUE ~ tournament.prizepool.currency # Keep original value if no match.
  )
)

# Check that it worked
unique(esport.data$tournament.prizepool.currencycode)

######################################################################################
# Import and clean historical exchange rate data
######################################################################################

# Read historical exchange rates data
historical_exchange_rates <- fread("historical_exchange_rates.csv")

# Separate currency abbreviaton from full name
historical_exchange_rates <- separate(historical_exchange_rates, "CURRENCY:Currency", 
into = c("currency_code", "currency_name"), sep = ":", extra = "merge")

# Check if all currencies are supported. All are except Croatian Kuna.
unique(historical_exchange_rates$currency_code)

# This isn't really a huge deal though because only two rows contain prize money in Croatian Kuna.
length(which(esport.data$tournament.prizepool.currencycode == "HRK"))

# Remove HRK (Croatian Kuna) from esport.data as there is no conversion data for it
esport.data <- esport.data %>%
  filter(!(tournament.prizepool.currencycode == "HRK" & !is.na(tournament.prizepool.amount)))

# Create a list of currency codes in esport.data
valid_currency_codes <- c("USD", "CNY", "EUR", "BRL", "PLN", "INR", "SEK", "GBP", 
                          "KZT", "RUB", "ARS", "NOK", "DKK", "CZK", "AUD", "CHF", 
                          "TRY", "JPY", "HRK", "VND", "ISK", "QAR", "MNT", "UAH", 
                          "IRR", "ZAR", "RSD", "BGN")

# Filter historical_exchange_rates to keep only rows with currencies in this list
filtered_exchange_rates <- historical_exchange_rates %>%
  filter(currency_code %in% valid_currency_codes)

# Convert the "begin_date" column to a Date object in esport.data
esport.data$begin_date <- as.Date(esport.data$begin_date)

# As date column in exchange rates dataset has inconsistent dates, use function to standardise format
filtered_exchange_rates$consistent_date <- as.Date(anytime(filtered_exchange_rates$`TIME_PERIOD:Time period or range`))
filtered_exchange_rates <- subset(filtered_exchange_rates,lubridate::year(consistent_date)>2015)

######################################################################################
# Join datasets to convert currency values
######################################################################################

# extract relevant columns
exchange <- filtered_exchange_rates[c("currency_code","OBS_VALUE:Observation Value","consistent_date")]
exchange <- exchange %>% rename(ObsValue=`OBS_VALUE:Observation Value`)
exchange$ObsValue <- ifelse(exchange$ObsValue=="NaN",NA,exchange$ObsValue)
exchange <- exchange %>% distinct(currency_code, consistent_date, .keep_all = TRUE)

merged_data <- esport.data %>%
  left_join(exchange, join_by("tournament.prizepool.currencycode" == "currency_code", closest(begin_date <= consistent_date)))

# Make a column based on date diff calculation
merged_data$date_diff <- difftime(merged_data$begin_date, merged_data$consistent_date, units = "days")

# Check it worked properly, seems like it worked perfectly (max is 0 days, min is -25 days)
max(na.omit(merged_data$date_diff))
min(na.omit(merged_data$date_diff))

# Convert prizepool amounts to USD using the exchange rates
merged_data <- mutate(merged_data, prizepool_usd = tournament.prizepool.amount / ObsValue)

######################################################################################
# Extract and categorize tournament stage info from tournament.name column
######################################################################################

merged_data <- merged_data %>% mutate(tournament.stage = case_when(
  grepl("Group", tournament.name) ~ "Group Stage",
  grepl("Playoff", tournament.name) ~ "Playoffs",
  grepl("playoff", tournament.name) ~ "Playoffs",
  grepl("Final", tournament.name) ~ "Finals",
  grepl("final", tournament.name) ~ "Finals",
  grepl("Qualifier", tournament.name) ~ "Qualifiers",
  grepl("Play-In", tournament.name) ~ "Play-Ins",
  grepl("Play-in", tournament.name) ~ "Play-Ins",
  grepl("Regular Season", tournament.name) ~ "Regular Season",
  grepl("Regular season", tournament.name) ~ "Regular Season",
  grepl("Placement", tournament.name) ~ "Placements",
  .default = "Other"
  )
)

unique(merged_data$tournament.stage)

# view(head(merged_data, 10000))

######################################################################################
# Import, transform and merge GDP per capita data
######################################################################################

# Import
GDP <- read_csv('historical_GDP.csv')

# Transform
GDP <- GDP %>% rename(country_name = `GDP per capita, current prices
 (U.S. dollars per capita)`)

# Historical exchange rates
historical_exchange_rates <- separate(historical_exchange_rates, "REF_AREA:Reference area", 
into = c("country_code", "country_name"), sep = ":", extra = "merge")

historical_exchange_rates$consistent_date <- as.Date(anytime(historical_exchange_rates$`TIME_PERIOD:Time period or range`))
historical_exchange_rates$year <- format(as.Date(historical_exchange_rates$consistent_date, format ="%Y/%m/%d"), "%Y")

historical_exchange_rates <- subset(historical_exchange_rates, year > 2015)

historical_exchange_rates$country_name <- trimws(historical_exchange_rates$country_name)

view(head(historical_exchange_rates))

# Have to manually change Turkey beforehand since its name had characters incompatible with case_when
GDP$country_name <- replace(GDP$country_name, 182, "Türkiye")
GDP$country_name <- replace(GDP$country_name, 170, "São Tomé and Príncipe")
GDP$country_name <- replace(GDP$country_name, 46, "Côte d'Ivoire")
view(GDP)

GDP <- GDP %>% mutate(across(-c(country_name), as.numeric))

GDP_long <- GDP %>% pivot_longer(cols = where(is.numeric), names_to = "year")

GDP_long <- subset(GDP_long, year > 2015 & year < 2024)
unique(GDP_long$year)

GDP_long <- GDP_long %>% rename(GDP = value)

unique(GDP_long$country_name)

# Back to GDP dataset
non_matching_countries <- setdiff(historical_exchange_rates$country_name, GDP_long$country_name)
other_way <- setdiff(GDP_long$country_name, historical_exchange_rates$country_name)

GDP_long <- GDP_long %>% mutate(country_name = case_when(
  grepl("Slovak Republic", country_name) ~ "Slovakia",
  grepl("China, People's Republic of", country_name) ~ "China",
  grepl("Russian Federation", country_name) ~ "Russia",
  grepl("Czech Republic", country_name) ~ "Czechia",
  grepl("Bahamas, The", country_name) ~ "The Bahamas",
  grepl("Congo, Republic of", country_name) ~ "Republic of Congo",
  grepl("Korea, Republic of", country_name) ~ "Korea",
  grepl("Congo, Dem. Rep. of the", country_name) ~ "Democratic Republic of the Congo",
  grepl("Saint Vincent and the Grenadines", country_name) ~ "St Vincent and the Grenadines",
  grepl("South Sudan, Republic of", country_name) ~ "South Sudan",
  grepl("Saint Lucia", country_name) ~ "St Lucia",
  grepl("Gambia, The", country_name) ~ "The Gambia",
  grepl("Saint Kitts and Nevis", country_name) ~ "St Kitts and Nevis",
  grepl("Brunei Darussalam", country_name) ~ "Brunei",
  grepl("Lao P.D.R.", country_name) ~ "Laos",
  grepl("Taiwan Province of China", country_name) ~ "Chinese Taipei",
  .default = country_name
  )
)

non_matching_countries <- setdiff(historical_exchange_rates$country_name, GDP_long$country_name)

# Merge GDP and exchange to get country codes
GDP_merge <- GDP_long %>% left_join(historical_exchange_rates, join_by("country_name", "year"), multiple = "any")

GDP_merge <- GDP_merge[c("country_code", "country_name", "year", "GDP")]

GDP_merge <- GDP_merge[!is.na(GDP_merge$country_code),]

# Process merged esport.data
merged_data$year <- format(as.Date(merged_data$begin_date, format ="%Y/%m/%d"), "%Y")

# Merge
main_GDP_merge <- merged_data %>%
  left_join(GDP_merge, join_by("opponent_0.location" == "country_code", "year"), multiple = "any")

main_GDP_merge <- main_GDP_merge %>% rename(
  opp0_GDP = GDP, opp0_country_name = country_name, opp0_year = year
)

secondary_GDP_merge <- merged_data %>%
  left_join(GDP_merge, join_by("opponent_1.location" == "country_code", "year"), multiple = "any")

main_GDP_merge$opp1_year <- secondary_GDP_merge$year
main_GDP_merge$opp1_country_name <- secondary_GDP_merge$country_name
main_GDP_merge$opp1_GDP <- secondary_GDP_merge$GDP

# view(head(main_GDP_merge, 1000))

# Now just need to add GDP data to this dataframe below.

######################################################################################
# Create new dataframe
######################################################################################

main_GDP_merge <- main_GDP_merge[1:10000,]

R=nrow(main_GDP_merge)
glmm.esportdata=vector(R,mode="list")
for(i in 1:R){
  main_GDP_merge$seq=seq(from=1,to=nrow(main_GDP_merge),by=1)
  previous.interactions=filter(main_GDP_merge,seq<i)
  
  if(main_GDP_merge$assigned.focal[i]=="opp0"){
    index=main_GDP_merge$index[i]
    season=main_GDP_merge$Season[i]
    tournament=main_GDP_merge$tournament_id[i]
    stage=main_GDP_merge$tournament.stage[i]
    match=main_GDP_merge$id[i]
    money=main_GDP_merge$prizepool_usd[i]
    money=ifelse(is.na(money),0,money)
    tier=main_GDP_merge$tournament.tier[i]
    winner=main_GDP_merge$game_winner_id[i]
    focal=main_GDP_merge$opponent_0.id[i]
    opponent=main_GDP_merge$opponent_1.id[i]
    win.f=ifelse(main_GDP_merge$opponent_0.id[i]==main_GDP_merge$game_winner_id[i],1,0)
    win.o=ifelse(main_GDP_merge$opponent_1.id[i]==main_GDP_merge$game_winner_id[i],1,0)
    location.f=main_GDP_merge$opponent_0.location[i]
    location.o=main_GDP_merge$opponent_1.location[i]
    GDP.f=main_GDP_merge$opp0_GDP[i]
    GDP.o=main_GDP_merge$opp1_GDP[i]
    # zDays.f=main_GDP_merge$zDays.h[i]
    # zDays.o=main_GDP_merge$zDays.a[i]
    
    
  } else {
    index=main_GDP_merge$index[i]
    season=main_GDP_merge$Season[i]
    tournament=main_GDP_merge$tournament_id[i]
    stage=main_GDP_merge$tournament.stage[i]
    match=main_GDP_merge$id[i]
    money=main_GDP_merge$prizepool_usd[i]
    money=ifelse(is.na(money),0,money)
    tier=main_GDP_merge$tournament.tier[i]
    winner=main_GDP_merge$game_winner_id[i]
    focal=main_GDP_merge$opponent_1.id[i]
    opponent=main_GDP_merge$opponent_0.id[i]
    win.f=ifelse(main_GDP_merge$opponent_1.id[i]==main_GDP_merge$game_winner_id[i],1,0)
    win.o=ifelse(main_GDP_merge$opponent_0.id[i]==main_GDP_merge$game_winner_id[i],1,0)
    location.f=main_GDP_merge$opponent_1.location[i]
    location.o=main_GDP_merge$opponent_0.location[i]
    GDP.f=main_GDP_merge$opp1_GDP[i]
    GDP.o=main_GDP_merge$opp0_GDP[i]
    # zDays.f=main_GDP_merge$zDays.a[i]
    # zDays.o=main_GDP_merge$zDays.h[i]
    
    
  }
  
  glmm.esportdata[[i]]=data.frame(index,season,tournament,stage,match,money,tier,winner,focal,opponent,win.f,win.o,location.f,location.o,GDP.f,GDP.o)
}


glmm.esportdata=do.call(rbind,glmm.esportdata)
glmm.esportdata=as_tibble(glmm.esportdata)
glmm.esportdata

# view(head(glmm.esportdata, 100))


#########################################################################################
#Calculate and add data for previous interactions to the dataframe
#########################################################################################

### CHECK THIS FUNCTION IS WORKING CORRECTLY!!! ###
###Function to extract data for the previous nth interaction
previous_interaction=function(glmm.df,nth.previous){
  R=nrow(glmm.df)
  previouswin.f=numeric(R)
  previouswin.o=numeric(R)
  previousloss.f=numeric(R)
  previousloss.o=numeric(R)
  previousmoney.f=numeric(R)
  previousmoney.o=numeric(R)
  previousGDP.f=numeric(R)
  previousGDP.o=numeric(R)
  for(i in 1:R){
    team.f=as.character(glmm.df$focal[i])
    team.o=as.character(glmm.df$opponent[i])
    earlier.times=filter(glmm.df,index<glmm.df$index[i])
    previous.games.f=filter(earlier.times,focal==team.f|opponent==team.f)
    previous.games.o=filter(earlier.times,focal==team.o|opponent==team.o)
    
    if(nrow(previous.games.f)>=nth.previous){
      nth.last.interaction=previous.games.f[nrow(previous.games.f)-(nth.previous-1),] #select the previous nth game
      previouswin.f[i]=ifelse(nth.last.interaction$focal==team.f&nth.last.interaction$win.f==1|nth.last.interaction$opponent==team.f&nth.last.interaction$win.o==1,1,0)
      previousloss.f[i]=ifelse(nth.last.interaction$focal==team.f&nth.last.interaction$win.f==0|nth.last.interaction$opponent==team.f&nth.last.interaction$win.o==0,1,0)
      previousmoney.f[i]=nth.last.interaction$money
      previousGDP.f[i]=ifelse(nth.last.interaction$focal==team.f,nth.last.interaction$GDP.f,nth.last.interaction$GDP.o)
    } else {
      previouswin.f[i]=NA
      previousloss.f[i]=NA
      previousmoney.f[i]=NA
      previousGDP.f[i]=NA
    }
    
    if(nrow(previous.games.o)>=nth.previous){
      nth.last.interaction=previous.games.o[nrow(previous.games.o)-(nth.previous-1),] #select the previous nth game
      previouswin.o[i]=ifelse(nth.last.interaction$focal==team.o&nth.last.interaction$win.f==1|nth.last.interaction$opponent==team.o&nth.last.interaction$win.o==1,1,0)
      previousloss.o[i]=ifelse(nth.last.interaction$focal==team.o&nth.last.interaction$win.f==0|nth.last.interaction$opponent==team.o&nth.last.interaction$win.o==0,1,0)
      previousmoney.o[i]=nth.last.interaction$money
      previousGDP.o[i]=ifelse(nth.last.interaction$focal==team.o,nth.last.interaction$GDP.f,nth.last.interaction$GDP.o)
    } else {
      previouswin.o[i]=NA
      previousloss.o[i]=NA
      previousmoney.o[i]=NA
      previousGDP.o[i]=NA
    }
  }
  new.glmm.df=data.frame(glmm.df,previouswin.f,previouswin.o,previousloss.f,previousloss.o,previousmoney.f,previousmoney.o,previousGDP.f,previousGDP.o)
  names(new.glmm.df)[ncol(glmm.df)+1]=paste("previous",nth.previous,"win.f",sep=".")
  names(new.glmm.df)[ncol(glmm.df)+2]=paste("previous",nth.previous,"win.o",sep=".")
  names(new.glmm.df)[ncol(glmm.df)+3]=paste("previous",nth.previous,"loss.f",sep=".")
  names(new.glmm.df)[ncol(glmm.df)+4]=paste("previous",nth.previous,"loss.o",sep=".")
  names(new.glmm.df)[ncol(glmm.df)+5]=paste("previous",nth.previous,"money.f",sep=".")
  names(new.glmm.df)[ncol(glmm.df)+6]=paste("previous",nth.previous,"money.o",sep=".")
  names(new.glmm.df)[ncol(glmm.df)+7]=paste("previous",nth.previous,"GDP.f",sep=".")
  names(new.glmm.df)[ncol(glmm.df)+8]=paste("previous",nth.previous,"GDP.o",sep=".")
  return(new.glmm.df)

}

glmm.esportdata$season=as.factor(glmm.esportdata$season)
glmm.esportdata=split(glmm.esportdata,f = glmm.esportdata$season)
glmm.esportdata=lapply(glmm.esportdata,previous_interaction,nth.previous=1)
glmm.esportdata=do.call(rbind,glmm.esportdata)
glmm.esportdata=as_tibble(glmm.esportdata)
glmm.esportdata




##################################################################
#Model 1. The basic model
##################################################################

### NEED TO MODIFY THIS FUNCTION TO OBTAIN:
### (1) BY-TEAM MEAN PWIN (2) W/IN-TEAM DEVIATIONS FROM THIS
### (3) BY-TEAM MEAN PLOSS (4) W/IN-TEAM DEVIATIONS FROM THIS

#function to calculate by-team win and loss frequencies for previous.nth.interaction
WUC_previous.outcome=function(oneseason.glmmdata,nth.previous.interaction){
  win.name.f=paste("previous",nth.previous.interaction,"win","f",sep=".")
  win.name.o=paste("previous",nth.previous.interaction,"win","o",sep=".")
  win.f.index=which(names(oneseason.glmmdata)==win.name.f)
  win.o.index=which(names(oneseason.glmmdata)==win.name.o)
  loss.name.f=paste("previous",nth.previous.interaction,"loss","f",sep=".")
  loss.name.o=paste("previous",nth.previous.interaction,"loss","o",sep=".")
  loss.f.index=which(names(oneseason.glmmdata)==loss.name.f)
  loss.o.index=which(names(oneseason.glmmdata)==loss.name.o)
  
  current.ncol=ncol(oneseason.glmmdata)
  
  all.teams=unique(c(as.character(oneseason.glmmdata$focal),as.character(oneseason.glmmdata$opponent)))
  R=length(all.teams)
  team.win.mean=numeric(R)
  team.loss.mean=numeric(R)
  for(i in 1:R){
    team=all.teams[i]
    all.interactions.f=filter(oneseason.glmmdata,focal==team)
    all.interactions.o=filter(oneseason.glmmdata,opponent==team)
    
    all.interactions=data.frame(index=c(all.interactions.f$index,all.interactions.o$index),
                                previous.wins=unlist(c(all.interactions.f[,win.f.index],all.interactions.o[,win.o.index])),
                                previous.losses=unlist(c(all.interactions.f[,loss.f.index],all.interactions.o[,loss.o.index]))
    )
    
    team.win.mean[i]=mean(all.interactions$previous.wins,na.rm=TRUE)
    team.loss.mean[i]=mean(all.interactions$previous.losses,na.rm=TRUE)
  }
  
  # print(hist(team.win.mean))
  # print(hist(team.loss.mean))
  
  team.win.mean=data.frame(all.teams,team.win.mean.f=team.win.mean)
  oneseason.glmmdata=left_join(oneseason.glmmdata,team.win.mean,by=c("focal"="all.teams"))
  names(team.win.mean)[2]="team.win.mean.o"
  oneseason.glmmdata=left_join(oneseason.glmmdata,team.win.mean,by=c("opponent"="all.teams"))
  
  team.loss.mean=data.frame(all.teams,team.loss.mean.f=team.loss.mean)
  oneseason.glmmdata=left_join(oneseason.glmmdata,team.loss.mean,by=c("focal"="all.teams"))
  names(team.loss.mean)[2]="team.loss.mean.o"
  oneseason.glmmdata=left_join(oneseason.glmmdata,team.loss.mean,by=c("opponent"="all.teams"))
  
  team.win.dev.f=(unlist(oneseason.glmmdata[,win.f.index]-oneseason.glmmdata$team.win.mean.f))
  team.win.dev.o=(unlist(oneseason.glmmdata[,win.o.index]-oneseason.glmmdata$team.win.mean.o))
  team.win.dev.f=as.numeric(team.win.dev.f)
  team.win.dev.o=as.numeric(team.win.dev.o)
  team.loss.dev.f=(unlist(oneseason.glmmdata[,loss.f.index]-oneseason.glmmdata$team.loss.mean.f))
  team.loss.dev.o=(unlist(oneseason.glmmdata[,loss.o.index]-oneseason.glmmdata$team.loss.mean.o))
  team.loss.dev.f=as.numeric(team.loss.dev.f)
  team.loss.dev.o=as.numeric(team.loss.dev.o)
  
  oneseason.glmmdata=data.frame(oneseason.glmmdata,team.win.dev.f,team.win.dev.o,team.loss.dev.f,team.loss.dev.o)
  
  
  new.col.names=c(paste("team.win.mean.",nth.previous.interaction,".f",sep=""),
                  paste("team.win.mean.",nth.previous.interaction,".o",sep=""),
                  paste("team.loss.mean.",nth.previous.interaction,".f",sep=""),
                  paste("team.loss.mean.",nth.previous.interaction,".o",sep=""),
                  paste("team.win.dev.",nth.previous.interaction,".f",sep=""),
                  paste("team.win.dev.",nth.previous.interaction,".o",sep=""),
                  paste("team.loss.dev.",nth.previous.interaction,".f",sep=""),
                  paste("team.loss.dev.",nth.previous.interaction,".o",sep="")
                  
  )
  names(oneseason.glmmdata)[(current.ncol+1):ncol(oneseason.glmmdata)]=new.col.names
  
  
  return(oneseason.glmmdata)
}


glmm.esportdata=split(glmm.esportdata,f = glmm.esportdata$season)
glmm.esportdata=lapply(glmm.esportdata,WUC_previous.outcome,nth.previous.interaction=1)
glmm.esportdata=do.call(rbind,glmm.esportdata)
glmm.esportdata=as.tibble(glmm.esportdata)
glmm.esportdata

glmm.footienodraws=subset(glmm.esportdata,result!="D")
glmm.footienodraws=as.tibble(glmm.footienodraws)
glmm.footienodraws


# first, model match outcomes as function of CURRENT situation
model1=glmer(win.f~home+
               (1|focal)+(1|opponent),
             family="binomial",data=glmm.esportdata,
             control=glmerControl(optimizer="bobyqa")
)

summary(model1)

# now include effect of previous win/loss, separated from overall win/loss frequency
### POTENTIAL ISSUE: CURRENT GAME CONTRIBUTES TO TEAM.WIN.MEAN -- need to check with null data
model2=glmer(win.f~home+
               team.win.mean.1.f+team.win.dev.1.f+
               team.win.mean.1.o+team.win.dev.1.o+
               team.loss.mean.1.f+team.loss.dev.1.f+
               team.loss.mean.1.o+team.loss.dev.1.o+
               (1|focal)+(1|opponent),
             family="binomial",data=glmm.esportdata,
             control=glmerControl(optimizer="bobyqa")
)

summary(model2)

# include interactions with previous match venue, to see if this influences WL effects
model3=glmer(win.f~home+
               team.win.mean.1.f+previous.1.home.f*team.win.dev.1.f+
               team.win.mean.1.o+previous.1.home.o*team.win.dev.1.o+
               team.loss.mean.1.f+previous.1.home.f*team.loss.dev.1.f+
               team.loss.mean.1.o+previous.1.home.o*team.loss.dev.1.o+
               (1|focal)+(1|opponent),
             family="binomial",data=glmm.esportdata,
             control=glmerControl(optimizer="bobyqa")
)

summary(model3)

# include interactions with previous goal margin, to see if this influences WL effects
model4=glmer(win.f~home+
               team.win.mean.1.f+previous.1.margin.f*team.win.dev.1.f+
               team.win.mean.1.o+previous.1.margin.o*team.win.dev.1.o+
               team.loss.mean.1.f+previous.1.margin.f*team.loss.dev.1.f+
               team.loss.mean.1.o+previous.1.margin.o*team.loss.dev.1.o+
               (1|focal)+(1|opponent),
             family="binomial",data=glmm.esportdata,
             control=glmerControl(optimizer="bobyqa")
)

summary(model4)


# include interactions with previous goal margin and previous venue, to see if this influences WL effects
model5=glmer(win.f~home+
               team.win.mean.1.f+previous.1.home.f*previous.1.margin.f*team.win.dev.1.f+
               team.win.mean.1.o+previous.1.home.o*previous.1.margin.o*team.win.dev.1.o+
               team.loss.mean.1.f+previous.1.home.f*previous.1.margin.f*team.loss.dev.1.f+
               team.loss.mean.1.o+previous.1.home.o*previous.1.margin.o*team.loss.dev.1.o+
               (1|focal)+(1|opponent),
             family="binomial",data=glmm.esportdata,
             control=glmerControl(optimizer="bobyqa")
)

summary(model5)