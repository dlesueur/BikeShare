library(tidyverse)
library(tidymodels)
library(vroom)
library(skimr)
library(train_dataExplorer)
library(GGally)
library(ggplot2)

# EDA
train_data <- vroom("files/train.csv", )
train_data$weather <- as.factor(train_data$weather)
train_data$season <- as.factor(train_data$season)
glimpse(train_data)
skim(train_data)
plot_intro(train_data)
plot_correlation(train_data)
plot_bar(train_data)
plot_histogram(train_data)
plot_missing(train_data)
ggpairs(train_data)
head(train_data)

plot1 <- ggplot(data = train_data, mapping= aes(x = weather)) +
  geom_bar(aes(fill = weather)) +
  ggtitle("Bar Plot of Weather") +
  theme(plot.title = element_text(hjust = 0.5))


plot2 <- ggplot(data = train_data, mapping = aes(x=temp)) +
  geom_histogram(binwidth = 3, fill = 'orchid1', color = 'black') +
  ggtitle("Distribution of Temperature") +
  theme(plot.title = element_text(hjust = 0.5))

humidityplot <- ggplot(data = train_data, mapping= aes(x = humidity, y = count)) +
  geom_point() +
  geom_smooth(se=FALSE) + 
  ggtitle("Scatter Plot of Humidity") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot() +
  geom_boxplot(data = train_data, aes(x = registered, fill = "Registered")) +
  geom_boxplot(data = train_data, aes(x = casual, fill = "Casual")) +
  labs(x = "Working Day", y = "Count", fill = "Type") +
  ggtitle("Boxplot of Registered and Casual Counts by Working Day") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot() +
  geom_boxplot(data = train_data, aes(y = registered)) +
  geom_boxplot(data = train_data, aes(y = casual))

plot4 <- ggplot() +
  geom_point(data = train_data, mapping = aes(x = datetime, y = count, color = factor(workingday))) +
  ggtitle("Scatterplot of Daily Count Colored by Workingday") +
  theme(plot.title = element_text(hjust = 0.5))

library(patchwork)

(plot1 + plot2) / (humidityplot + plot4)



# Set up and Fit the Linear Model
model <- linear_reg() %>% # Type of model
  set_engine("lm") %>% # Engine = What R function to use
  set_mode("regression") %>% # Regression here means quantitative response
  fit(formula=log(count)~holiday+workingday+temp+atemp+humidity+windspeed+season+weather, data=train_data)


# read in test data
test_data <- vroom("files/test.csv")
test_data$season <- as.factor(test_data$season)
test_data$weather <- as.factor(test_data$weather)


## Generate Predictions Using Linear Model
bike_predictions <- predict(model,
                            new_data=test_data) # Use fit to predict
bike_predictions <- exp(bike_predictions) # back transform log



# format for kaggle
kaggle_submission <- bike_predictions %>%
bind_cols(., test_data) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

## Write out the file
vroom_write(x=kaggle_submission, file="./LinearPreds.csv", delim=",")


## POISSON LINEAR MODEL
library(poissonreg)

pois_model <- poisson_reg() %>%
  set_engine("glm") %>%
  set_mode("regression") %>%
  fit(formula = count~holiday+workingday+temp+atemp+humidity+windspeed+season+weather, data = train_data)

poisson_predictions <- predict(pois_model, 
                            new_data = test_data)
poisson_predictions

# format for kaggle
kaggle_submission_pois <- poisson_predictions %>%
  bind_cols(., test_data) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

## Write out the file
vroom_write(x=kaggle_submission_pois, file="./PoissonLinearPreds.csv", delim=",")



