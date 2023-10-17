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
  geom_histogram(color="#512d6d",fill="lightblue") + 
  xlab("Number of Runs by Boston") + 
  ylab("Count")
# Number of visitor runs
ra_2 <- boston_game_weather %>%
  ggplot(aes(x=vis_runs)) + 
  geom_histogram(color="#512d6d",fill="lightblue") + 
  xlab("Number of Runs by Visitors") + 
  ylab("Count")
# Boston runs - Visitor runs
ra_3 <- boston_game_weather %>%
  ggplot(aes(x=score_diff,fill=wl)) + 
  geom_histogram(color="#512d6d") + 
  scale_fill_manual(values = c("#E69F00","#009E73")) + 
  labs(x = "Boston Runs - Visitor Runs",
       y = "Count",
       fill="Win/Not")
# Win or Not Win
ra_4 <- boston_game_weather %>%
  ggplot(aes(x=wl,fill=wl)) + 
  geom_bar(color="#512d6d") + 
  scale_fill_manual(values = c("#E69F00","#009E73")) + 
  theme(legend.position = "none") +
  labs(x = "No Win or Win",
       y = "Count")

(ra_1 + ra_2) / (ra_4 + ra_3)

# Game durations
rb_1 <- boston_game_weather %>%
  ggplot(aes(x=duration)) + 
  geom_histogram(color="#512d6d",fill="lightblue") + 
  labs(x = "Game Duration",
       y = "Count")
# Game attendances
rb_2 <- boston_game_weather %>%
  ggplot(aes(x=attendance)) + 
  geom_histogram(color="#512d6d",fill="lightblue") + 
  labs(x = "Game Attendance",
       y = "Count")

(rb_1 + rb_2)

w_1 <- boston_game_weather %>%
  ggplot(aes(x=humid)) + 
  geom_histogram(color="#512d6d",fill="lightblue") + 
  labs(x = "Humidity",
       y = "Count")

w_2 <- boston_game_weather %>%
  ggplot(aes(x=tempF)) + 
  geom_histogram(color="#512d6d",fill="lightblue") + 
  labs(x = "Temperature",
       y = "Count")

w_3 <- boston_game_weather %>%
  ggplot(aes(x=precip)) + 
  geom_histogram(color="#512d6d",fill="lightblue") + 
  labs(x = "Precipitation",
       y = "Count")

(w_1 + w_2 + w_3)

boston_game_weather %>%
  select(-c(hm_tm,dbl_hdr,tempK,year,month,day,wl_binary)) %>%
  ggpairs()

boston_game_weather %>%
  group_by(year) %>%
  summarise(wins_by_year=sum(wl_binary)) %>%
  ggplot(aes(x=year,y=wins_by_year)) + 
  geom_point(size=2,color="darkgreen") + 
  geom_smooth(color="#512d6d",fill="lightblue")
  

boston_game_weather %>%
  group_by(year) %>%
  summarise(wins_by_year=sum(wl_binary)) %>%
  ggplot(aes(x=year,y=wins_by_year)) + 
  geom_point(size=2,color="darkgreen") + 
  geom_smooth(method = "lm",color="#512d6d",fill="lightblue")

boston_game_weather %>%
  ggplot(aes(x=date,y=humid)) + 
  geom_point(size=2,color="lightblue",alpha=0.5) + 
  geom_smooth(color="#512d6d",fill="darkgreen")

boston_game_weather %>%
  ggplot(aes(x=date,y=tempF)) + 
  geom_point(size=2,color="lightblue",alpha=0.5) + 
  geom_smooth(color="#512d6d",fill="darkgreen")

boston_game_weather %>%
  ggplot(aes(x=date,y=precip)) + 
  geom_point(size=2,color="lightblue",alpha=0.5) + 
  geom_smooth(color="#512d6d",fill="darkgreen")

boston_game_weather %>%
  ggplot(aes(x=date,y=humid)) + 
  geom_point(size=2,color="lightblue",alpha=0.5) + 
  geom_smooth(method = "lm",color="#512d6d",fill="darkgreen")

boston_game_weather %>%
  ggplot(aes(x=date,y=tempF)) + 
  geom_point(size=2,color="lightblue",alpha=0.5) + 
  geom_smooth(method = "lm",color="#512d6d",fill="darkgreen")

boston_game_weather %>%
  ggplot(aes(x=date,y=precip)) + 
  geom_point(size=2,color="lightblue",alpha=0.5) + 
  geom_smooth(method = "lm",color="#512d6d",fill="darkgreen")

boston_game_weather %>%
  group_by(year) %>%
  summarise(yearly_avg_tempF=mean(tempF)) %>%
  ggplot(aes(x=year,y=yearly_avg_tempF)) + 
  geom_point(size=2,color="darkgreen") + 
  geom_smooth(color="#512d6d",fill="lightblue")

boston_game_weather %>%
  group_by(year) %>%
  summarise(yearly_avg_humid=mean(humid)) %>%
  ggplot(aes(x=year,y=yearly_avg_humid)) + 
  geom_point(size=2,color="darkgreen") + 
  geom_smooth(color="#512d6d",fill="lightblue")

boston_game_weather %>%
  group_by(year) %>%
  summarise(yearly_avg_precip=mean(precip)) %>%
  ggplot(aes(x=year,y=yearly_avg_precip)) + 
  geom_point(size=2,color="darkgreen") + 
  geom_smooth(color="#512d6d",fill="lightblue")

boston_game_weather %>%
  ggplot(aes(x=humid,y=hm_runs)) + 
  geom_point(size=2,color="darkgreen",alpha=0.5)

boston_game_weather %>%
  ggplot(aes(x=tempF,y=hm_runs)) + 
  geom_point(size=2,color="lightblue",alpha=0.5) + 
  geom_smooth(color="#512d6d",fill="darkgreen")

boston_game_weather %>%
  ggplot(aes(x=precip,y=hm_runs)) + 
  geom_point(size=2,color="lightblue",alpha=0.5) + 
  geom_smooth(color="#512d6d",fill="darkgreen")

boston_game_weather %>%
  ggplot(aes(x=wl,y=hm_runs)) + 
  geom_boxplot()

boston_game_weather %>%
  ggplot(aes(x=wl,y=tempF)) + 
  geom_boxplot(color="#512d6d")

boston_game_weather %>%
  ggplot(aes(x=wl,y=humid)) + 
  geom_boxplot(color="#512d6d")

boston_game_weather %>%
  ggplot(aes(x=wl,y=precip)) + 
  geom_boxplot(color="#512d6d")

boston_game_weather %>%
  ggplot(aes(x=humid,y=attendance)) + 
  geom_point(size=2,color="lightblue",alpha=0.5) + 
  geom_smooth(color="#512d6d",fill="darkgreen")

boston_game_weather %>%
  ggplot(aes(x=tempF,y=attendance)) + 
  geom_point(size=2,color="lightblue",alpha=0.5) + 
  geom_smooth(color="#512d6d",fill="darkgreen")

boston_game_weather %>%
  ggplot(aes(x=precip,y=attendance)) + 
  geom_point(size=2,color="lightblue",alpha=0.5) + 
  geom_smooth(color="#512d6d",fill="darkgreen")

boston_game_weather %>%
  filter(attendance > 0) %>%
  ggplot(aes(x=humid,y=attendance)) + 
  geom_point(size=2,color="lightblue",alpha=0.5) + 
  geom_smooth(color="#512d6d",fill="darkgreen")

boston_game_weather %>%
  filter(attendance > 0) %>%
  ggplot(aes(x=tempF,y=attendance)) + 
  geom_point(size=2,color="lightblue",alpha=0.5) + 
  geom_smooth(color="#512d6d",fill="darkgreen")

boston_game_weather %>%
  filter(attendance > 0) %>%
  ggplot(aes(x=precip,y=attendance)) + 
  geom_point(size=2,color="lightblue",alpha=0.5) + 
  geom_smooth(color="#512d6d",fill="darkgreen")


boston_game_weather %>%
  ggplot(aes(x=humid,y=duration)) + 
  geom_point(size=2,color="lightblue",alpha=0.5) + 
  geom_smooth(color="#512d6d",fill="darkgreen")

boston_game_weather %>%
  ggplot(aes(x=tempF,y=duration)) + 
  geom_point(size=2,color="lightblue",alpha=0.5) + 
  geom_smooth(color="#512d6d",fill="darkgreen")

boston_game_weather %>%
  ggplot(aes(x=precip,y=duration)) + 
  geom_point(size=2,color="lightblue",alpha=0.5) + 
  geom_smooth(color="#512d6d",fill="darkgreen")

hs_date <- boston_game_weather %>%
  ggplot(aes(x=date,y=hm_runs)) + 
  geom_point(size=2,color="lightblue",alpha=0.5) + 
  geom_smooth(color="#512d6d",fill="darkgreen") + 
  labs(x="Date",y="Red Sox Runs")
hs_temp <- boston_game_weather %>%
  ggplot(aes(x=tempF,y=hm_runs)) + 
  geom_point(size=2,color="lightblue",alpha=0.5) + 
  geom_smooth(color="#512d6d",fill="darkgreen") + 
  labs(x="Temperature (F)",y="Red Sox Runs")
hs_humid <- boston_game_weather %>%
  ggplot(aes(x=humid,y=hm_runs)) + 
  geom_point(size=2,color="lightblue",alpha=0.5) + 
  geom_smooth(color="#512d6d",fill="darkgreen") + 
  labs(x="Humidity",y="Red Sox Runs")
hs_precip <- boston_game_weather %>%
  ggplot(aes(x=precip,y=hm_runs)) + 
  geom_point(size=2,color="lightblue",alpha=0.5) + 
  geom_smooth(color="#512d6d",fill="darkgreen") + 
  labs(x="Precipitable Water",y="Red Sox Runs")

(hs_date + hs_temp) / (hs_humid + hs_precip)

ws_date <- boston_game_weather %>%
  ggplot(aes(x=date,y=wl,color=wl)) + 
  geom_point(size=2,alpha=0.2) + 
  scale_color_manual(values = c("#E69F00","#009E73")) + 
  theme(legend.position = "none") + 
  labs(x="Date",y="Win/Not")
ws_temp <- boston_game_weather %>%
  ggplot(aes(x=tempF,y=wl,color=wl)) + 
  geom_point(size=2,alpha=0.2) + 
  scale_color_manual(values = c("#E69F00","#009E73")) +
  labs(x="Temperature (F)",y="Win/Not", color="Win/Not")
ws_humid <- boston_game_weather %>%
  ggplot(aes(x=humid,y=wl,color=wl)) + 
  geom_point(size=2,alpha=0.2) + 
  scale_color_manual(values = c("#E69F00","#009E73")) +
  theme(legend.position = "none") +
  labs(x="Humidity",y="Win/Not")
ws_precip <- boston_game_weather %>%
  ggplot(aes(x=precip,y=wl,color=wl)) + 
  geom_point(size=2,alpha=0.2) + 
  scale_color_manual(values = c("#E69F00","#009E73")) +
  theme(legend.position = "none") +
  labs(x="Precipitable Water",y="Win/Not")

(ws_date + ws_temp) / (ws_humid + ws_precip)

ds_date <- boston_game_weather %>%
  ggplot(aes(x=date,y=duration)) + 
  geom_point(size=2,color="lightblue",alpha=0.5) + 
  geom_smooth(color="#512d6d",fill="darkgreen") + 
  labs(x="Date",y="Game Duration")
ds_temp <- boston_game_weather %>%
  ggplot(aes(x=tempF,y=duration)) + 
  geom_point(size=2,color="lightblue",alpha=0.5) + 
  geom_smooth(color="#512d6d",fill="darkgreen") + 
  labs(x="Temperature (F)",y="Game Duration")
ds_humid <- boston_game_weather %>%
  ggplot(aes(x=humid,y=duration)) + 
  geom_point(size=2,color="lightblue",alpha=0.5) + 
  geom_smooth(color="#512d6d",fill="darkgreen") + 
  labs(x="Humidity",y="Game Duration")
ds_precip <- boston_game_weather %>%
  ggplot(aes(x=precip,y=duration)) + 
  geom_point(size=2,color="lightblue",alpha=0.5) + 
  geom_smooth(color="#512d6d",fill="darkgreen") + 
  labs(x="Precipitable Water",y="Game Duration")

(ds_date + ds_temp) / (ds_humid + ds_precip)

as_date <- boston_game_weather %>%
  filter(attendance > 0) %>%
  ggplot(aes(x=date,y=attendance)) + 
  geom_point(size=2,color="lightblue",alpha=0.5) + 
  geom_smooth(color="#512d6d",fill="darkgreen") + 
  labs(x="Date",y="Game Attendance")
as_temp <- boston_game_weather %>%
  filter(attendance > 0) %>%
  ggplot(aes(x=tempF,y=attendance)) + 
  geom_point(size=2,color="lightblue",alpha=0.5) + 
  geom_smooth(color="#512d6d",fill="darkgreen") + 
  labs(x="Temperature (F)",y="Game Attendance")
as_humid <- boston_game_weather %>%
  filter(attendance > 0) %>%
  ggplot(aes(x=humid,y=attendance)) + 
  geom_point(size=2,color="lightblue",alpha=0.5) + 
  geom_smooth(color="#512d6d",fill="darkgreen") + 
  labs(x="Humidity",y="Game Attendance")
as_precip <- boston_game_weather %>%
  filter(attendance > 0) %>%
  ggplot(aes(x=precip,y=attendance)) + 
  geom_point(size=2,color="lightblue",alpha=0.5) + 
  geom_smooth(color="#512d6d",fill="darkgreen") + 
  labs(x="Precipitable Water",y="Game Attendance")

(as_date + as_temp) / (as_humid + as_precip)

boston_game_weather %>%
  group_by(year) %>%
  summarise(wins_by_year=sum(wl_binary)) %>%
  ggplot(aes(x=year,y=wins_by_year)) + 
  geom_point(size=2,color="darkgreen") + 
  geom_smooth(color="#512d6d",fill="lightblue") + 
  labs(x="Year",y="Number of Wins")

wa_temp <- boston_game_weather %>%
  ggplot(aes(x=date,y=tempF)) + 
  geom_point(size=2,color="lightblue",alpha=0.5) + 
  geom_smooth(color="#512d6d",fill="darkgreen") + 
  labs(x="Date",y="Temperature")
wb_temp <- boston_game_weather %>%
  group_by(year) %>%
  summarise(mean_temp=mean(tempF)) %>%
  ggplot(aes(x=year,y=mean_temp)) + 
  geom_point(size=2,color="darkgreen",alpha=0.5) + 
  geom_smooth(color="#512d6d",fill="lightblue") + 
  labs(x="Year",y="Mean Temperature")

wa_humid <- boston_game_weather %>%
  ggplot(aes(x=date,y=humid)) + 
  geom_point(size=2,color="lightblue",alpha=0.5) + 
  geom_smooth(color="#512d6d",fill="darkgreen") + 
  labs(x="Date",y="Humidity")
wb_humid <- boston_game_weather %>%
  group_by(year) %>%
  summarise(mean_humid=mean(humid)) %>%
  ggplot(aes(x=year,y=mean_humid)) + 
  geom_point(size=2,color="darkgreen",alpha=0.5) + 
  geom_smooth(color="#512d6d",fill="lightblue") + 
  labs(x="Year",y="Mean Humidity")

wa_precip <- boston_game_weather %>%
  ggplot(aes(x=date,y=precip)) + 
  geom_point(size=2,color="lightblue",alpha=0.5) + 
  geom_smooth(color="#512d6d",fill="darkgreen") + 
  labs(x="Date",y="Percipitable Water")
wb_precip <- boston_game_weather %>%
  group_by(year) %>%
  summarise(mean_precip=mean(precip)) %>%
  ggplot(aes(x=year,y=mean_precip)) + 
  geom_point(size=2,color="darkgreen",alpha=0.5) + 
  geom_smooth(color="#512d6d",fill="lightblue") + 
  labs(x="Year",y="Mean Percipitable Water")

(wa_temp + wb_temp) / (wa_humid + wb_humid) / (wa_precip + wb_precip)

prop_wins <- boston_game_weather %>%
  group_by(year) %>%
  summarise(games_played=n(), games_won = sum(wl_binary)) %>%
  mutate(win_pct = games_won / games_played)

prop_wins %>%
  ggplot(aes(x=year,y=win_pct)) + 
  geom_point(size=2,color="darkgreen",alpha=0.5) + 
  geom_smooth(color="#512d6d",fill="lightblue") + 
  labs(x="Year",y="Win Percentage")


