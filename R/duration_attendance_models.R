# load useful packages
library(tidyverse)
library(tidymodels)
library(parttree)
library(vip)
library(RColorBrewer) # for plot colors
library(rpart.plot)
library(patchwork)

# set protocols and themes
theme_set(theme_minimal(base_size = 12))
tidymodels_prefer()

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
         wl_binary=ifelse(wl=="W",1,0),
         date_num=as.numeric(date)/1000)

# check results
glimpse(boston_game_weather)

###### Linear Models for Duration and Attendance #######

### Attendance
boston_game_weather_att <- boston_game_weather %>%
  filter(attendance > 0) %>%
  mutate(attendance_log = log10(attendance))

attend_intervals <- reg_intervals(attendance ~ humid + precip + tempF + date_num,
                                  data=boston_game_weather_att,
                                  keep_reps = TRUE)

p_attend_coeffs <- attend_intervals %>%
  ggplot(aes(x=.estimate,y=term)) + 
  geom_point(size=2) + 
  geom_errorbar(aes(xmin=.lower,xmax=.upper),width=0.1) + 
  geom_vline(xintercept = 0.0,linetype = "dashed")

p_attend_ints <- attend_intervals %>%
  unnest(.replicates) %>%
  ggplot(aes(estimate)) +
  geom_histogram(bins = 30,color="white") +
  facet_wrap( ~ term, scales = "free") +
  geom_vline(aes(xintercept = .lower), data = attend_intervals, col = "blue") +
  geom_vline(aes(xintercept = .upper), data = attend_intervals, col = "blue")

p_attend_coeffs + p_attend_ints

### Duration

duration_intervals <- reg_intervals(duration ~ humid + precip + tempF + date_num + vis_runs + hm_runs + wl,
                                    data=boston_game_weather,
                                    keep_reps = TRUE)

duration_intervals %>%
  ggplot(aes(x=.estimate,y=term)) + 
  geom_point(size=2) + 
  geom_errorbar(aes(xmin=.lower,xmax=.upper),width=0.1) + 
  geom_vline(xintercept = 0.0,linetype = "dashed")

duration_intervals %>%
  unnest(.replicates) %>%
  ggplot(aes(estimate)) +
  geom_histogram(bins = 30,color="white") +
  facet_wrap( ~ term, scales = "free") +
  geom_vline(aes(xintercept = .lower), data = duration_intervals, col = "blue") +
  geom_vline(aes(xintercept = .upper), data = duration_intervals, col = "blue")



###### Tree Models for Duration and Attendance #######

### Attendance

# create test/train splits
boston_split <- initial_split(boston_game_weather_att, strata = year)
boston_train <- training(boston_split)
boston_test <- testing(boston_split)
# create cv folds
boston_folds <- vfold_cv(boston_train)
# define model specification
tree_spec <- decision_tree(
  cost_complexity = tune(),
  tree_depth = tune(),
  min_n = tune()) %>%
  set_engine("rpart") %>%
  set_mode("regression")
# create tuning grid for hyperparameters
tree_grid <- grid_regular(cost_complexity(), 
                          tree_depth(), 
                          min_n(),
                          levels = 5)
# set up workflow
boston_recipe <- recipe(attendance_log ~ humid + precip + tempF + date_num, data = boston_train)

boston_wf <- workflow() %>%
  add_recipe(boston_recipe)
# tune
tree_rs <- boston_wf %>%
  add_model(tree_spec) %>%
  tune_grid(resamples = boston_folds,
            grid = tree_grid,
            metrics = metric_set(rmse,mae)
  )

autoplot(tree_rs)

show_best(tree_rs, "rmse")

select_best(tree_rs, "rmse")

show_best(tree_rs, "mae")

select_best(tree_rs, "mae")

final_tree <- finalize_model(tree_spec, select_best(tree_rs, "mae"))

final_tree_fit <- fit(final_tree, attendance_log ~ humid + precip + tempF + date_num, boston_train)

final_tree_fit %>%
  vip(geom = "col", aesthetics = list(fill = "midnightblue", alpha = 0.8)) +
  scale_y_continuous(expand = c(0, 0))

# refit with just two predictors
ex_fit <- fit(final_tree,attendance_log ~ tempF + date_num, boston_train)


boston_train %>%
  ggplot(aes(x=date,y=tempF)) + 
  geom_parttree(data=ex_fit,aes(fill=attendance_log),alpha=0.3) + 
  geom_point(aes(color=attendance_log),alpha=0.7) + 
  scale_color_viridis_c(aesthetics = c("color","fill"))

boston_test %>%
  ggplot(aes(x=date,y=tempF)) + 
  geom_parttree(data=ex_fit,aes(fill=attendance_log),alpha=0.3) + 
  geom_point(aes(color=attendance_log),alpha=0.7) + 
  scale_color_viridis_c(aesthetics = c("color","fill"))

final_tree_fit %>%
  predict(boston_test) %>%
  cbind(boston_test %>% select(attendance_log,date)) %>%
  ggplot(aes(attendance_log, .pred,size=date)) +
  geom_abline(slope = 1, lty = 2, color = "gray50", alpha = 0.5) +
  geom_point(alpha = 0.6, color = "midnightblue") +
  coord_fixed() + 
  xlim(c(2.75,4.75)) + ylim(c(2.75,4.75)) + 
  labs(x = "Observed Attendance (log)", y = "Predicted Attendance (log)")

#tree_fit_rpart <- extract_fit_engine(final_tree_fit)
#rpart.plot(tree_fit_rpart,roundint=FALSE)

### Duration

# create test/train splits
boston_split <- initial_split(boston_game_weather, strata = year)
boston_train <- training(boston_split)
boston_test <- testing(boston_split)
# create cv folds
boston_folds <- vfold_cv(boston_train)
# define model specification
tree_spec <- decision_tree(
  cost_complexity = tune(),
  tree_depth = tune(),
  min_n = tune()) %>%
  set_engine("rpart") %>%
  set_mode("regression")
# create tuning grid for hyperparameters
tree_grid <- grid_regular(cost_complexity(), 
                          tree_depth(), 
                          min_n(),
                          levels = 5)
# set up workflow
boston_recipe <- recipe(duration ~ humid + precip + tempF + date_num + vis_runs + hm_runs + wl, data = boston_train)

boston_wf <- workflow() %>%
  add_recipe(boston_recipe)
# tune
tree_rs <- boston_wf %>%
  add_model(tree_spec) %>%
  tune_grid(resamples = boston_folds,
            grid = tree_grid,
            metrics = metric_set(rmse,mae)
  )

autoplot(tree_rs)

show_best(tree_rs, "rmse")

select_best(tree_rs, "rmse")

show_best(tree_rs, "mae")

select_best(tree_rs, "mae")

final_tree <- finalize_model(tree_spec, select_best(tree_rs, "mae"))

final_tree_fit <- fit(final_tree, duration ~ humid + precip + tempF + date_num + vis_runs + hm_runs + wl, boston_train)

final_tree_fit %>%
  vip(geom = "col", aesthetics = list(fill = "midnightblue", alpha = 0.8)) +
  scale_y_continuous(expand = c(0, 0))

# refit with just two predictors
ex_fit <- fit(final_tree,duration ~ date_num + vis_runs, boston_train)


boston_train %>%
  ggplot(aes(x=date,y=vis_runs)) + 
  geom_parttree(data=ex_fit,aes(fill=duration),alpha=0.3) + 
  geom_point(aes(color=duration),alpha=0.7) + 
  scale_color_viridis_c(aesthetics = c("color","fill"))


boston_test %>%
  ggplot(aes(x=date,y=vis_runs)) + 
  geom_parttree(data=ex_fit,aes(fill=duration),alpha=0.3) + 
  geom_point(aes(color=duration),alpha=0.7) + 
  scale_color_viridis_c(aesthetics = c("color","fill"))

tree_fit_rpart <- extract_fit_engine(final_tree_fit)
rpart.plot(tree_fit_rpart,roundint=FALSE)
