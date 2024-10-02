library(tidyverse)
library(tidymodels)
library(vroom)
library(skimr)
library(GGally)
library(ggplot2)
library(glmnet)

train_data <- vroom("files/train.csv", ) # read in data

# remove registered and casual columns, change count to log(count)
train_data <- train_data %>%
  select(-registered, -casual)

# write recipe for data wrangling
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
  step_select(-datetime) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())

bike_recipe2 <- recipe(count ~ ., data = train_data) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_nominal_predictors())
  
prepped <- prep(bike_recipe)
prepped2 <- prep(bike_recipe2)
bake(prepped, new_data=train_data)
# define a model
poisson_model <- poisson_reg() %>% # Type of model
  set_engine("glm") %>% # Engine = What R function to use
  set_mode("regression") 

# penalized linear regression model
preg_model <- linear_reg(penalty=3, mixture=0.5) %>%
  set_engine("glmnet")

# combine into a workflow
bike_flow <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(poisson_model) %>%
  fit(data=train_data)

preg_wf <- workflow() %>%
  add_recipe(bike_recipe2) %>%
  add_model(preg_model) %>%
  fit(data=train_data)

# read in test data
test_data <- vroom("files/test.csv")


# run all the steps on test data
penalized_preds <- predict(preg_wf, new_data=test_data)








test_data$season <- as.factor(test_data$season)
test_data$weather <- as.factor(test_data$weather)


## Generate Predictions Using Linear Model
bike_predictions <- predict(model,
                            new_data=test_data) # Use fit to predict
bike_predictions <- exp(bike_predictions) # back transform log



# format for kaggle
kaggle_submission <- pois_preds %>%
bind_cols(., test_data) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

## Write out the file
vroom_write(x=kaggle_submission, file="./RecipePoissonPreds.csv", delim=",")


# ## POISSON LINEAR MODEL
# library(poissonreg)
# 
# pois_model <- poisson_reg() %>%
#   set_engine("glm") %>%
#   set_mode("regression") %>%
#   fit(formula = count~holiday+workingday+temp+atemp+humidity+windspeed+season+weather, data = train_data)
# 
# poisson_predictions <- predict(pois_model, 
#                             new_data = test_data)
# poisson_predictions
# 
# # format for kaggle
# kaggle_submission_pois <- poisson_predictions %>%
#   bind_cols(., test_data) %>% #Bind predictions with test data
#   select(datetime, .pred) %>% #Just keep datetime and prediction variables
#   rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
#   mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
#   mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle
# 
# ## Write out the file
# vroom_write(x=kaggle_submission_pois, file="./PoissonLinearPreds.csv", delim=",")



