library(tidyverse)
library(tidymodels)
library(vroom)
library(skimr)
library(GGally)
library(ggplot2)
library(glmnet)
library(stacks)

train_data <- vroom("files/train.csv") 
test_data <- vroom("files/test.csv")

train_data <- train_data %>%
  select(-registered, -casual) %>%
  mutate(count = log(count))

# recipe
bike_recipe <- recipe(count ~ ., data = train_data) %>%
  step_time(datetime, features = "hour") %>%
  step_date(datetime, features = "year") %>%
  step_mutate(
    weather = if_else(weather == 4, 3, weather),
    weather = as.factor(weather),
    season = as.factor(season),
    workingday = as.factor(workingday),
    datetime_year = as.factor(datetime_year)
  ) %>%
  step_rm(datetime) %>%  
  step_interact(terms = ~ workingday:datetime_hour) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())


# BART Model
bart_model <- bart(mode = "regression", 
                   trees = 100) %>%
  set_engine("dbarts")

# workflow
bart_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(bart_model) %>%
  fit(data = train_data)

# # set up grid of tuning values
# tuning_params <- grid_regular(prior_terminal_node_coef(),
#                               prior_terminal_node_expo(),
#                               prior_outcome_range(),
#                               levels = 5)
# 
# # cross validation
# folds <- vfold_cv(train_data, v = 6, repeats=1)
# 
# bart_models <- bart_wf %>%
#   tune_grid(resamples = folds,
#             grid = tuning_params, 
#             metrics = metric_set(rmse, mae, rsq))


# tuned wf

# predict
bart_predictions <- predict(bart_wf, new_data = test_data)

kaggle_submission <- bart_predictions %>%
  bind_cols(., test_data) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count = exp(count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

vroom_write(x=kaggle_submission, file="./BartPreds.csv", delim=",")
