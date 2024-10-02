library(tidyverse)
library(tidymodels)
library(vroom)
library(skimr)
library(GGally)
library(ggplot2)
library(glmnet)
library(rpart)

# read in data
train_data <- vroom("files/train.csv") 
test_data <- vroom("files/test.csv")

train_data <- train_data %>%
  select(-registered, -casual) %>%
  mutate(count = log(count))

# write recipe
bike_recipe <- recipe(count ~ ., data = train_data) %>%
  step_mutate(weather = if_else(weather == 4, 3, weather)) %>%
  step_mutate(weather = as.factor(weather)) %>%
  step_mutate(season = as.factor(season)) %>%
  step_mutate(workingday = as.factor(workingday)) %>%
  step_mutate(holiday = as.factor(holiday)) %>%
  step_time(datetime, features = "hour") %>%
  step_mutate(datetime_hour = as.factor(datetime_hour)) %>%
  step_date(datetime, features = "month") %>%
  step_mutate(datetime_month = as.factor(datetime_month)) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_rm(datetime)

# prepped_recipe <- prep(bike_recipe, training = train_data)
# baked_recipe <- bake(prepped_recipe, new_data = train_data)

# define the model
preg_model <- linear_reg(penalty=0, mixture=0.25) %>%
  set_engine("glmnet")

# create a workflow
preg_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(preg_model) %>%
  fit(data=train_data)

# calculate predictions
penalized_preds <- predict(preg_wf, new_data=test_data)

# format for Kaggle and save csv
kaggle_submission <- penalized_preds %>%
  bind_cols(., test_data) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count = exp(count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

vroom_write(x=kaggle_submission, file="./PenalizedPreds.csv", delim=",")

