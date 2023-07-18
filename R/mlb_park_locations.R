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

# add info about open roof
baseball_parks$Open_Roof <- c("No",rep("Yes",9),
                              "No","Yes","Yes","Yes",
                              "No","No",
                              rep("Yes",7),"No","Yes",
                              "Yes","No","No","No","Yes")

ggplot() +
  geom_polygon(data=states_df,
               aes(long,lat,group=group),
               fill="white",color="darkgrey") + 
  geom_point(data=baseball_parks,aes(x=Longitude,y=Latitude),
             color="darkblue",size=2.2) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) + 
  xlab("") + ylab("")

ggplot() +
  geom_polygon(data=states_df,
               aes(long,lat,group=group),
               fill="white",color="darkgrey") + 
  geom_point(data=baseball_parks,aes(x=Longitude,y=Latitude,color=Open_Roof),
             size=2.5) +
  scale_color_manual(values=c("orange","darkblue")) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) + 
  xlab("") + ylab("")

ggplot() +
  geom_polygon(data=states_df,
               aes(long,lat,group=group),
               fill="white",color="darkgrey") + 
  geom_point(data=baseball_parks,aes(x=Longitude,y=Latitude,color=Open_Roof),
             size=2.5) +
  geom_point(data=baseball_parks %>% filter(Abbreviation == "BOS"),
                                            aes(x=Longitude,y=Latitude),size=5,color="darkblue") + 
  geom_label( 
    data=baseball_parks %>% filter(Abbreviation == "BOS"), # Filter data first
    aes(label="Fenway",x=-71,y=43.5),color = "black",
    fill="red") + 
  scale_color_manual(values=c("orange","darkblue")) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) + 
  xlab("") + ylab("")



