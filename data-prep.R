require(tidyverse)
WLdata <- read.csv("csgo_data_game_filtered.csv",na.strings=c(""," ","NA"))
WLdata <- as_tibble(WLdata)
WLdatalong <- WLdata %>%
  pivot_longer(cols = contains(".winner.id"),
               names_to = "game",
               names_pattern = "game_(.*).winner.id",
               values_to = "game_winner_id",
               values_drop_na = TRUE
  )