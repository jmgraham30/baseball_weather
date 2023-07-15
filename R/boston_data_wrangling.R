# load necessary packages
library(tidyverse) # for data wrangling

# load the raw data to work with
boston_home_games <- read_csv("data/boston_home.csv") # Boston home games
boston_weather <- read_csv("data/boston_weather_df.csv") # Boston weather

# glimpse data frames
glimpse(boston_home_games)
glimpse(boston_weather)

# since each data frame has different dates, we need
# to find the common date values
range(boston_home_games$date)
range(boston_weather$date)

# first, filter the Boston home game dates before 1950-04-01
boston_home_games_50 <- boston_home_games %>%
  filter(date >= min(boston_weather$date))

# check result
glimpse(boston_home_games_50)
range(boston_home_games_50$date)

# extract weather observations for days that there was a Boston home game
boston_home_game_weather <- boston_weather %>%
  filter(date %in% boston_home_games_50$date)

# check result
glimpse(boston_home_game_weather)
range(boston_home_game_weather$date)

# combine the data frames into one 
boston_game_weather <- left_join(boston_home_games_50,boston_home_game_weather,by="date")

# check result
glimpse(boston_game_weather)

# let's also convert temperature to Fahrenheit degrees:
# conversion function
k_to_f <- function(temp_K){
  
  temp_F <- 32 + 9/5*(temp_K - 273.15)
  
  return(temp_F)
  
}

# apply conversion function
boston_game_weather <- boston_game_weather %>%
  mutate(tempF=k_to_f(tempK))

# check result
glimpse(boston_game_weather)

# save the result
write_csv(boston_game_weather,"data/boston_game_weather.csv")



