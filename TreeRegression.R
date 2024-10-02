library(tidyverse)
library(tidymodels)
library(vroom)
library(skimr)
library(GGally)
library(ggplot2)
library(glmnet)

# read in data
train_data <- vroom("files/train.csv") 
test_data <- vroom("files/test.csv")

train_data <- train_data %>%
  select(-registered, -casual) %>%
  mutate(count = log(count))

# write recipe
bike_recipe <- recipe(count ~ ., data = train_data) %>%
  step_time(datetime, features = "hour") %>%
  step_date(datetime, features = "month") %>%
  step_mutate(
    weather = if_else(weather == 4, 3, weather),
    weather = as.factor(weather),
    season = as.factor(season),
    workingday = as.factor(workingday),
    holiday = as.factor(holiday), 
    datetime_hour = as.factor(datetime_hour), 
    datetime_month = as.factor(datetime_month)
  ) %>%
  step_rm(datetime) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())
  

# define model
tree_model <- decision_tree(tree_depth = tune(), 
                            cost_complexity = tune(),
                            min_n = tune()) %>%
  set_engine("rpart") %>%
  set_mode("regression")

# create workflow
tree_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(tree_model)

# set up grid of tuning values
tuning_params <- grid_regular(tree_depth(), 
                              cost_complexity(),
                              min_n(),
                              levels = 5)

# set up k-fold CV
folds <- vfold_cv(train_data, v = 6, repeats=1)

CV_results <- tree_wf %>%
  tune_grid(resamples=folds,
            grid=tuning_params,
            metrics=metric_set(rmse, mae, rsq))

# find best tuning params
bestTune <- CV_results %>%
  select_best(metric = "rmse")

# finalize workflow and make predictions
tree_model <- decision_tree(tree_depth = 15, 
                            cost_complexity = 0.00000316,
                            min_n = 21) %>%
  set_engine("rpart") %>%
  set_mode("regression")

tree_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(tree_model) %>%
  fit(data=train_data)

tree_preds <- predict(tree_wf, new_data=test_data)

kaggle_submission <- tree_preds %>%
  bind_cols(., test_data) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count = exp(count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

vroom_write(x=kaggle_submission, file="./TreePreds.csv", delim=",")
