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