# load packages
library(tidyverse) # wrangling, plots etc.
library(GGally) # paired plots
library(skimr) # data summary
library(patchwork) # for combining plots
library(RColorBrewer) # for plot colors

# set plot theme
theme_set(theme_minimal(base_size = 12))

# load data
boston_game_weather <- read_csv("data/boston_game_weather.csv")

# glimpse
glimpse(boston_game_weather)

# overall data summary
skim(boston_game_weather)

# create separate columns with day, month, and year corresponding
# to game dates; and create a binary win (1) or not (0) column  
boston_game_weather <- boston_game_weather %>%
  mutate(year=year(date),
         month=month(date),
         day=day(date),
         wl_binary=ifelse(wl=="W",1,0))

# check results
glimpse(boston_game_weather)

#### initial exploratory plots
# start with potential response variables

# Number of Boston runs
ra_1 <- boston_game_weather %>%
  ggplot(aes(x=hm_runs)) + 
  geom_histogram(color="black",fill="lightblue") + 
  xlab("Number of Runs by Boston") + 
  ylab("Count")
# Number of visitor runs
ra_2 <- boston_game_weather %>%
  ggplot(aes(x=vis_runs)) + 
  geom_histogram(color="black",fill="lightblue") + 
  xlab("Number of Runs by Visitors") + 
  ylab("Count")
# Boston runs - Visitor runs
ra_3 <- boston_game_weather %>%
  ggplot(aes(x=score_diff,fill=wl)) + 
  geom_histogram(color="black") + 
  scale_fill_manual(values = c("#E69F00","#009E73")) + 
  labs(x = "Boston Runs - Visitor Runs",
       y = "Count",
       fill="Win/Not")
# Win or Not Win
ra_4 <- boston_game_weather %>%
  ggplot(aes(x=wl,fill=wl)) + 
  geom_bar(color="black") + 
  scale_fill_manual(values = c("#E69F00","#009E73")) + 
  theme(legend.position = "none") +
  labs(x = "No Win or Win",
       y = "Count")

r_1 <- (ra_1 + ra_2) / (ra_4 + ra_3)

# Game durations
rb_1 <- boston_game_weather %>%
  ggplot(aes(x=duration)) + 
  geom_histogram(color="black",fill="lightblue") + 
  labs(x = "Game Duration",
       y = "Count")
# Game attendances
rb_2 <- boston_game_weather %>%
  ggplot(aes(x=attendance)) + 
  geom_histogram(color="black",fill="lightblue") + 
  labs(x = "Game Attendance",
       y = "Count")

r_2 <- (rb_1 + rb_2)

w_1 <- boston_game_weather %>%
  ggplot(aes(x=humid)) + 
  geom_histogram(color="black",fill="lightblue") + 
  labs(x = "Humidity",
       y = "Count")

w_2 <- boston_game_weather %>%
  ggplot(aes(x=tempF)) + 
  geom_histogram(color="black",fill="lightblue") + 
  labs(x = "Temperature",
       y = "Count")

w_3 <- boston_game_weather %>%
  ggplot(aes(x=precip)) + 
  geom_histogram(color="black",fill="lightblue") + 
  labs(x = "Precipitation",
       y = "Count")

(w_1 + w_2 + w_3)

boston_game_weather %>%
  select(-c(hm_tm,dbl_hdr,vis_runs,score_diff,tempK,year,month,day,wl_binary)) %>%
  ggpairs()

boston_game_weather %>%
  group_by(year) %>%
  summarise(wins_by_year=sum(wl_binary)) %>%
  ggplot(aes(x=year,y=wins_by_year)) + 
  geom_point() + 
  geom_smooth()

boston_game_weather %>%
  group_by(year) %>%
  summarise(wins_by_year=sum(wl_binary)) %>%
  ggplot(aes(x=year,y=wins_by_year)) + 
  geom_point() + 
  geom_smooth(method = "lm")

boston_game_weather %>%
  ggplot(aes(x=date,y=humid)) + 
  geom_point() + 
  geom_smooth()

boston_game_weather %>%
  ggplot(aes(x=date,y=tempF)) + 
  geom_point() + 
  geom_smooth()

boston_game_weather %>%
  ggplot(aes(x=date,y=precip)) + 
  geom_point() + 
  geom_smooth()

boston_game_weather %>%
  ggplot(aes(x=date,y=humid)) + 
  geom_point() + 
  geom_smooth(method = "lm")

boston_game_weather %>%
  ggplot(aes(x=date,y=tempF)) + 
  geom_point() + 
  geom_smooth(method = "lm")

boston_game_weather %>%
  ggplot(aes(x=date,y=precip)) + 
  geom_point() + 
  geom_smooth(method = "lm")

boston_game_weather %>%
  group_by(year) %>%
  summarise(yearly_avg_tempF=mean(tempF)) %>%
  ggplot(aes(x=year,y=yearly_avg_tempF)) + 
  geom_point() + 
  geom_smooth()

boston_game_weather %>%
  group_by(year) %>%
  summarise(yearly_avg_humid=mean(humid)) %>%
  ggplot(aes(x=year,y=yearly_avg_humid)) + 
  geom_point() + 
  geom_smooth()

boston_game_weather %>%
  group_by(year) %>%
  summarise(yearly_avg_precip=mean(precip)) %>%
  ggplot(aes(x=year,y=yearly_avg_precip)) + 
  geom_point() + 
  geom_smooth()

boston_game_weather %>%
  ggplot(aes(x=humid,y=hm_runs)) + 
  geom_point() + 
  geom_smooth()

boston_game_weather %>%
  ggplot(aes(x=tempF,y=hm_runs)) + 
  geom_point() + 
  geom_smooth()

boston_game_weather %>%
  ggplot(aes(x=precip,y=hm_runs)) + 
  geom_point() + 
  geom_smooth()

boston_game_weather %>%
  ggplot(aes(x=wl,y=hm_runs)) + 
  geom_boxplot()

boston_game_weather %>%
  ggplot(aes(x=wl,y=tempF)) + 
  geom_boxplot()

boston_game_weather %>%
  ggplot(aes(x=wl,y=humid)) + 
  geom_boxplot()

boston_game_weather %>%
  ggplot(aes(x=wl,y=precip)) + 
  geom_boxplot()

boston_game_weather %>%
  ggplot(aes(x=humid,y=attendance)) + 
  geom_point() + 
  geom_smooth()

boston_game_weather %>%
  ggplot(aes(x=tempF,y=attendance)) + 
  geom_point() + 
  geom_smooth()

boston_game_weather %>%
  ggplot(aes(x=precip,y=attendance)) + 
  geom_point() + 
  geom_smooth()

boston_game_weather %>%
  filter(attendance > 0) %>%
  ggplot(aes(x=humid,y=attendance)) + 
  geom_point() + 
  geom_smooth()

boston_game_weather %>%
  filter(attendance > 0) %>%
  ggplot(aes(x=tempF,y=attendance)) + 
  geom_point() + 
  geom_smooth()

boston_game_weather %>%
  filter(attendance > 0) %>%
  ggplot(aes(x=precip,y=attendance)) + 
  geom_point() + 
  geom_smooth()


boston_game_weather %>%
  ggplot(aes(x=humid,y=duration)) + 
  geom_point() + 
  geom_smooth()

boston_game_weather %>%
  ggplot(aes(x=tempF,y=duration)) + 
  geom_point() + 
  geom_smooth()

boston_game_weather %>%
  ggplot(aes(x=precip,y=duration)) + 
  geom_point() + 
  geom_smooth()

