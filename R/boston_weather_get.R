# load necessary packages
library(tidyverse) # for wrangling
library(lubridate) # for date manipulations
library(RNCEP) # to download weather data

# download air temperature data around Boston 
# for April - October, 1950 - 2022
wx.extent1 <- NCEP.gather.surface(variable='air.sig995',
                          months.minmax=c(4,10), years.minmax=c(1950,2022),
                          lat.minmax=c(41,42), lon.minmax=c(71,72))

# aggregate temperatures to daily mean
mean_temps <- NCEP.aggregate(wx.extent1,HOURS = FALSE,fxn = "mean")

# convert to a data frame
mean_temps_df <- NCEP.array2df(mean_temps)

# average over location
mean_temps_df <- mean_temps_df %>%
  group_by(datetime) %>%
  summarise(tempK=mean(variable1))

# download humidity data around Boston 
# for April - October, 1950 - 2022
wx.extent2 <- NCEP.gather.surface(variable='rhum.sig995',
                                  months.minmax=c(4,10), years.minmax=c(1950,2022),
                                  lat.minmax=c(41,42), lon.minmax=c(71,72))

# aggregate humidity to daily mean
mean_humid <- NCEP.aggregate(wx.extent2,HOURS = FALSE,fxn = "mean")

# convert to a data frame
mean_humid_df <- NCEP.array2df(mean_humid)

# average over location
mean_humid_df <- mean_humid_df %>%
  group_by(datetime) %>%
  summarise(humid=mean(variable1))

# combine temp and humidity observations into a single
# data frame
weather_df <- left_join(mean_temps_df,mean_humid_df)

# remove unnecessary part of datetime specifications 
weather_df$date <- str_remove_all(weather_df$datetime,"[_X]")

# covert date variables to year-month-day format
weather_df$date <- ymd(weather_df$date)

# remove irrelevant column
weather_df %>%
  select(-datetime) %>%
  write_csv("boston_weather_df.csv")
