require(tidyverse)
WLdata <- read.csv("csgo_data_game_filtered.csv", # read in CSV file
                   na.strings=c(""," ","NA")) # replace blank cells with NA
WLdata <- as_tibble(WLdata)
WLdatalong <- WLdata %>%
  pivot_longer(cols = contains(".winner.id"),
               names_to = "game",
               names_pattern = "game_(.*).winner.id",
               values_to = "game_winner_id",
               values_drop_na = TRUE
  )