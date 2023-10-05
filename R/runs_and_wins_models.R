# load useful packages
library(tidyverse)
library(tidymodels)
library(parttree)
library(vip)
library(RColorBrewer) # for plot colors


# set protocols and themes
theme_set(theme_minimal(base_size = 12))
tidymodels_prefer()
doParallel::registerDoParallel()

# load the data
boston_game_weather <- read_csv("data/boston_game_weather.csv")

# glimpse
glimpse(boston_game_weather)

# create separate columns with day, month, and year corresponding
# to game dates; and create a binary win (1) or not (0) column 
boston_game_weather <- boston_game_weather %>%
  mutate(year=year(date),
         month=month(date),
         day=day(date),
         wl_binary=ifelse(wl=="W",1,0))

# check results
glimpse(boston_game_weather)


### Models for Wins/Losses

fit_lr_boots <- function(split){
  glm(wl_binary ~ date + vis_runs + humid + precip + tempF,
      family = binomial,
      data=analysis(split))
}

boots <- bootstraps(boston_game_weather, times = 2000, strata = year)

boot_models <-
  boots %>% 
  mutate(model = map(splits, fit_lr_boots),
         coef_info = map(model, tidy))

boot_coefs <- 
  boot_models %>% 
  unnest(coef_info)

percentile_intervals <- int_pctl(boot_models, coef_info)

ggplot(boot_coefs, aes(estimate)) +
  geom_histogram(bins = 30,color="white") +
  facet_wrap( ~ term, scales = "free") +
  geom_vline(aes(xintercept = .lower), data = percentile_intervals, col = "blue") +
  geom_vline(aes(xintercept = .upper), data = percentile_intervals, col = "blue")

boot_aug <- 
  boot_models %>% 
  sample_n(150) %>% 
  mutate(augmented = map(model, augment)) %>% 
  unnest(augmented)

ggplot(boot_aug, aes(vis_runs, wl_binary)) +
  geom_line(aes(y = exp(.fitted)/(1+exp(.fitted)), group = id), col = "gray") +
  geom_jitter(height=0.025,aes(color=factor(wl_binary))) + 
  scale_color_manual(values = c("#E69F00","#009E73")) + 
  labs(x="Number of Visitor Runs Scores", y="Win Likelihood",
       color="Win/Loss")

