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