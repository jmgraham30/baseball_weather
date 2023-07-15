# load packages
library(tidyverse) # wrangling, plots etc.
library(GGally) # paired plots
library(skimr) # data summary

# set plot theme
theme_set(theme_minimal(base_size = 12))

# load data
boston_game_weather <- read_csv("data/boston_game_weather.csv")

# glimpse
glimpse(boston_game_weather)

# overall data summary
skim(boston_game_weather)

# some manipulations
boston_game_weather <- boston_game_weather %>%
  mutate(year=year(date),
         month=month(date),
         day=day(date),
         wl_binary=ifelse(wl=="W",1,0))

# check results
glimpse(boston_game_weather)

# initial plots
boston_game_weather %>%
  ggplot(aes(x=hm_runs)) + 
  geom_histogram()

boston_game_weather %>%
  ggplot(aes(x=vis_runs)) + 
  geom_histogram()

boston_game_weather %>%
  ggplot(aes(x=wl)) + 
  geom_bar()

boston_game_weather %>%
  ggplot(aes(x=duration)) + 
  geom_histogram()

boston_game_weather %>%
  ggplot(aes(x=attendance)) + 
  geom_histogram()

boston_game_weather %>%
  ggplot(aes(x=humid)) + 
  geom_histogram()

boston_game_weather %>%
  ggplot(aes(x=tempF)) + 
  geom_histogram()

boston_game_weather %>%
  select(-c(hm_tm,dbl_hdr,vis_runs,score_diff,tempK,year,month,day,wl_binary)) %>%
  ggpairs()
