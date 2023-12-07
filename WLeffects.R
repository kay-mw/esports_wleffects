require(tidyverse)
require(lme4)
require(MCMCglmm)
require(parallel)

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

R=nrow(esport.data)
glmm.esportdata=vector(R,mode="list") 
for(i in 1:R){
  esport.data$seq=seq(from=1,to=nrow(esport.data),by=1)
  previous.interactions=filter(esport.data,seq<i)
  
  if(esport.data$assigned.focal[i]=="h"){
    index=esport.data$index[i]
    season=esport.data$Season[i]
    tier=esport.data$tier[i]
    result=esport.data$result[i]
    focal=esport.data$home[i]
    opponent=esport.data$visitor[i]
    win.f=esport.data$hWin[i]
    win.o=esport.data$aWin[i]
    home=1
    margin=esport.data$margin[i]
    zDays.f=esport.data$zDays.h[i]
    zDays.o=esport.data$zDays.a[i]
    
    
  } else {
    index=esport.data$index[i]
    season=esport.data$Season[i]
    tier=esport.data$tier[i]
    result=esport.data$result[i]
    focal=esport.data$visitor[i]
    opponent=esport.data$home[i]
    win.f=esport.data$aWin[i]
    win.o=esport.data$hWin[i]
    home=0
    margin=esport.data$margin[i]
    zDays.f=esport.data$zDays.a[i]
    zDays.o=esport.data$zDays.h[i]
    
    
  }
  
  glmm.esportdata[[i]]=data.frame(index,season,tier,result,focal,opponent,win.f,win.o,home,margin,zDays.f,zDays.o)
}


glmm.esportdata=do.call(rbind,glmm.esportdata)
glmm.esportdata=as_tibble(glmm.esportdata)
glmm.esportdata


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
  previoushome.f=numeric(R)
  previoushome.o=numeric(R)
  previousmargin.f=numeric(R)
  previousmargin.o=numeric(R)
  for(i in 1:R){
    team.f=as.character(glmm.df$focal[i])
    team.o=as.character(glmm.df$opponent[i])
    earlier.times=filter(glmm.df,index<glmm.df$index[i])
    previous.games.f=filter(earlier.times,focal==team.f|opponent==team.f)
    previous.games.o=filter(earlier.times,focal==team.o|opponent==team.o)
    
    if(nrow(previous.games.f)>=nth.previous){
      nth.last.interaction=previous.games.f[nrow(previous.games.f)-(nth.previous-1),] #select the previous nth game
      previouswin.f[i]=ifelse(nth.last.interaction$focal==team.f&nth.last.interaction$win.f==1&nth.last.interaction$win.o==0|nth.last.interaction$opponent==team.f&nth.last.interaction$win.f==0&nth.last.interaction$win.o==1,1,0)
      previousloss.f[i]=ifelse(nth.last.interaction$focal==team.f&nth.last.interaction$win.f==0&nth.last.interaction$win.o==1|nth.last.interaction$opponent==team.f&nth.last.interaction$win.f==1&nth.last.interaction$win.o==0,1,0)
      previoushome.f[i]=ifelse(nth.last.interaction$focal==team.f,nth.last.interaction$home,1-nth.last.interaction$home)
      previousmargin.f[i]=nth.last.interaction$margin
    } else {
      previouswin.f[i]=NA
      previousloss.f[i]=NA
      previoushome.f[i]=NA
      previousmargin.f[i]=NA
    }
    
    if(nrow(previous.games.o)>=nth.previous){
      nth.last.interaction=previous.games.o[nrow(previous.games.o)-(nth.previous-1),] #select the previous nth game
      previouswin.o[i]=ifelse(nth.last.interaction$focal==team.o&nth.last.interaction$win.f==1&nth.last.interaction$win.o==0|nth.last.interaction$opponent==team.o&nth.last.interaction$win.f==0&nth.last.interaction$win.o==1,1,0)
      previousloss.o[i]=ifelse(nth.last.interaction$focal==team.o&nth.last.interaction$win.f==0&nth.last.interaction$win.o==1|nth.last.interaction$opponent==team.o&nth.last.interaction$win.f==1&nth.last.interaction$win.o==0,1,0)
      previoushome.o[i]=ifelse(nth.last.interaction$focal==team.o,nth.last.interaction$home,1-nth.last.interaction$home)
      previousmargin.o[i]=nth.last.interaction$margin
    } else {
      previouswin.o[i]=NA
      previousloss.o[i]=NA
      previoushome.o[i]=NA
      previousmargin.o[i]=NA
    }
  }
  new.glmm.df=data.frame(glmm.df,previouswin.f,previouswin.o,previousloss.f,previousloss.o,previoushome.f,previoushome.o,previousmargin.f,previousmargin.o)
  names(new.glmm.df)[ncol(glmm.df)+1]=paste("previous",nth.previous,"win.f",sep=".")
  names(new.glmm.df)[ncol(glmm.df)+2]=paste("previous",nth.previous,"win.o",sep=".")
  names(new.glmm.df)[ncol(glmm.df)+3]=paste("previous",nth.previous,"loss.f",sep=".")
  names(new.glmm.df)[ncol(glmm.df)+4]=paste("previous",nth.previous,"loss.o",sep=".")
  names(new.glmm.df)[ncol(glmm.df)+5]=paste("previous",nth.previous,"home.f",sep=".")
  names(new.glmm.df)[ncol(glmm.df)+6]=paste("previous",nth.previous,"home.o",sep=".")
  names(new.glmm.df)[ncol(glmm.df)+7]=paste("previous",nth.previous,"margin.f",sep=".")
  names(new.glmm.df)[ncol(glmm.df)+8]=paste("previous",nth.previous,"margin.o",sep=".")
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