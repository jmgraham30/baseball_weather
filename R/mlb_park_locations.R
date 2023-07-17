# load packages
library(tidyverse) # for plotting and data wrangling

# set plot theme
theme_set(theme_minimal(base_size = 12))

# load sports venue data
pro_sports_venues <- read_csv("data/USProSportsVenues.csv")

# glimpse
glimpse(pro_sports_venues)

# load map data for US states minus AK and HI
states_df <- map_data("state") %>%
  filter(region != "alaska" | region != "hawaii")

# glimpse
glimpse(states_df)

# check states
length(table(states_df$region))
table(states_df$region)

# extract baseball parks
baseball_parks <- pro_sports_venues %>%
  filter(Sport == "MLB")

ggplot() +
  geom_polygon(data=states_df,
               aes(long,lat,group=group),
               fill="white",color="darkgrey") + 
  geom_point(data=baseball_parks,aes(x=Longitude,y=Latitude),
             color="darkblue",size=2)
