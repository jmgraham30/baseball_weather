library(tidyverse)
library(lubridate)
library(RNCEP)


wx.extent1 <- NCEP.gather.surface(variable='air.sig995',
                          months.minmax=c(4,10), years.minmax=c(1950,2022),
                          lat.minmax=c(41,42), lon.minmax=c(289,290))

mean_temps <- NCEP.aggregate(wx.extent1,HOURS = FALSE,fxn = "mean")

mean_temps_df <- NCEP.array2df(mean_temps)

mean_temps_df <- mean_temps_df %>%
  group_by(datetime) %>%
  summarise(tempK=mean(variable1))

wx.extent2 <- NCEP.gather.surface(variable='rhum.sig995',
                                  months.minmax=c(4,10), years.minmax=c(1950,2022),
                                  lat.minmax=c(41,42), lon.minmax=c(289,290))

mean_humid <- NCEP.aggregate(wx.extent2,HOURS = FALSE,fxn = "mean")

mean_humid_df <- NCEP.array2df(mean_humid)

mean_humid_df <- mean_humid_df %>%
  group_by(datetime) %>%
  summarise(humid=mean(variable1))

wx.extent3 <- NCEP.gather.surface(variable='pr_wtr.eatm',
                                  months.minmax=c(4,10), years.minmax=c(1950,2022),
                                  lat.minmax=c(41,42), lon.minmax=c(289,290))

mean_precip <- NCEP.aggregate(wx.extent3,HOURS = FALSE,fxn = "mean")

mean_precip_df <- NCEP.array2df(mean_precip)

mean_precip_df <- mean_precip_df %>%
  group_by(datetime) %>%
  summarise(precip=mean(variable1))

weather_df <- left_join(mean_temps_df,mean_humid_df)

weather_df <- left_join(weather_df,mean_precip_df)

weather_df$date <- str_remove_all(weather_df$datetime,"[_X]")

weather_df$date <- ymd(weather_df$date)

glimpse(weather_df)

weather_df %>%
  select(-datetime) %>%
  write_csv("boston_weather_df.csv")
