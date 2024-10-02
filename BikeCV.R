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

# define the model
preg_model <- linear_reg(penalty=tune(), 
                         mixture=tune()) %>%
  set_engine("glmnet")

# create a workflow
preg_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(preg_model)

# grid of values to tune over
grid_of_tuning_params <- grid_regular(penalty(), 
                                      mixture(), 
                                      levels = 5)

## Split data for CV
folds <- vfold_cv(train_data, v = 6, repeats=1)

CV_results <- preg_wf %>%
tune_grid(resamples=folds,
          grid=grid_of_tuning_params,
          metrics=metric_set(rmse, mae, rsq)) #Or leave metrics NULL

## Plot Results (example)
collect_metrics(CV_results) %>% # Gathers metrics into DF
  filter(.metric=="rmse") %>%
ggplot(data=., aes(x=penalty, y=mean, color=factor(mixture))) +
geom_line()

## Find Best Tuning Parameters
bestTune <- CV_results %>%
  select_best(metric = "rmse")




