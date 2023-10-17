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


##

win_loss_intervals <- reg_intervals(wl_binary ~ date + vis_runs + humid + precip + tempF,
                                    data=boston_game_weather,
                                    model_fn = "glm",
                                    family = "binomial",
                                    type = "percentile",
                                    keep_reps = TRUE)


runs_intervals <- reg_intervals(hm_runs ~ date + vis_runs + humid + precip + tempF,
                                    data=boston_game_weather,
                                    model_fn = "lm",
                                    type = "percentile",
                                    keep_reps = TRUE)


win_loss_intervals %>% ggplot(aes(x=.estimate,y=term)) + 
  geom_point(size=2) + 
  geom_errorbar(aes(xmin=.lower,xmax=.upper),width=0.1) + 
  geom_vline(xintercept = 0.0,linetype = "dashed") +
  labs(x="Coefficient Estimate",y="Predictor",title="Win/Loss Model Coefficients") 


runs_intervals %>% ggplot(aes(x=.estimate,y=term)) + 
  geom_point(size=2) + 
  geom_errorbar(aes(xmin=.lower,xmax=.upper),width=0.1) + 
  geom_vline(xintercept = 0.0,linetype = "dashed") +
  labs(x="Coefficient Estimate",y="Predictor",title="Boston Runs Model Coefficients") 

runs_intervals %>%
  unnest(.replicates) %>%
  ggplot(aes(estimate)) +
  geom_histogram(bins = 30,color="white") +
  facet_wrap( ~ term, scales = "free") +
  geom_vline(aes(xintercept = .lower), data = runs_intervals, col = "blue") +
  geom_vline(aes(xintercept = .upper), data = runs_intervals, col = "blue")


############################ Useless ##############################

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
                          levels = 10)
# set up workflow
boston_recipe <- recipe(hm_runs ~ humid + precip + tempF, data = boston_train)

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

final_tree_fit <- fit(final_tree, hm_runs ~ humid + precip + tempF, boston_train)

final_tree_fit %>%
  vip(geom = "col", aesthetics = list(fill = "midnightblue", alpha = 0.8)) +
  scale_y_continuous(expand = c(0, 0))

# refit with just two predictors
ex_fit <- fit(final_tree,hm_runs ~ tempF + precip, boston_train)


boston_train %>%
  ggplot(aes(x=tempF,y=precip)) + 
  geom_parttree(data=ex_fit,aes(fill=hm_runs),alpha=0.3) + 
  geom_point(aes(color=hm_runs),alpha=0.7) + 
  scale_color_viridis_c(aesthetics = c("color","fill"))

boston_test %>%
  ggplot(aes(x=tempF,y=precip)) + 
  geom_parttree(data=ex_fit,aes(fill=hm_runs),alpha=0.3) + 
  geom_point(aes(color=hm_runs),alpha=0.7) + 
  scale_color_viridis_c(aesthetics = c("color","fill"))
