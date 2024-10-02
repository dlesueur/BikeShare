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
  step_date(datetime, features = "month") %>%
  step_time(datetime, features = "hour") %>%
  step_mutate(
    weather = if_else(weather == 4, 3, weather),
    weather = as.factor(weather),
    season = as.factor(season),
    workingday = as.factor(workingday),
    holiday = as.factor(holiday), 
    datetime_month = as.factor(datetime_month)
  ) %>%
  step_rm(datetime) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())

# define model
forest_mod <- rand_forest(mtry = tune(), 
                         min_n = tune(),
                         trees = 500) %>%
  set_engine("ranger") %>%
  set_mode("regression")

# create workflow
forest_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(forest_mod)

# set up grid of tuning values
forest_tuning_params <- grid_regular(mtry(range = c(1,10)),
                              min_n(),
                              levels = 5)
# set up k-fold CV
folds <- vfold_cv(train_data, v = 5, repeats=1)

CV_results <- forest_wf %>%
  tune_grid(resamples=folds,
            grid=forest_tuning_params,
            metrics=metric_set(rmse, mae, rsq))

# find best tuning params
bestTuneForest <- CV_results %>%
  select_best(metric = "rmse")



# finalize workflow and make predictions
forest_model <- rand_forest(mtry = 10, 
                            min_n = 2,
                            trees = 500) %>%
  set_engine("ranger") %>%
  set_mode("regression")

forest_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(forest_model) %>%
  fit(data=train_data)

forest_preds <- predict(forest_wf, new_data=test_data)

kaggle_submission <- forest_preds %>%
  bind_cols(., test_data) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count = exp(count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

vroom_write(x=kaggle_submission, file="./ForestPreds.csv", delim=",")

