install.packages("rpart")

library(tidyverse)
library(caTools)
library(e1071)
library(rpart)
# classification trees and regression trees
# data is split into leaves based on information entropy algorithm

dt <- read_csv("Position_Salaries.csv")

dt <- dt %>% 
  select(2:3)
summary(dt)
glimpse(dt)

dt

# Split the dataset into training and test set
# set.seed(123)
# split <- sample.split(dt$Salary, SplitRatio = 0.8)
# training <- subset(dt, split = T)
# test <- subset(dt, split =F)

# feature Scaling

# training <- scale(training)
# test <- scale(test)


# Fitting Decision Tree Regression to the dataset

regressor <- rpart(formula = Salary ~ .,
                   data = dt,
                   control = rpart.control(minsplit = 1))
regressor
summary(regressor)

# Predicting a new result

y_pred <- predict(regressor, 
                  data.frame(Level = 6.5))
y_pred

# Visualising the Decision Tree Regression results
# this visualisation is for linear continuous regression model

# ggplot()+
#   geom_point(aes(dt$Level,dt$Salary),
#              shape = 21,
#              color = '#890000',
#              size = 2.5)+
#   geom_line(aes(dt$Level, y = predict(regressor, newdata = dt)),
#             color = 'darkblue',
#             size = 0.8,
#             linetype = "dotted")+
#   labs(
#     title = 'Decision Tree Regression',
#     subtitle = 'by Paul Juverdeanu',
#     x = 'Level',
#     y = 'Salary'
#   )+
#   theme_light()+
#   theme(
#     plot.title = element_text(face = 'bold', size = 14, color = '#006400'),
#     plot.subtitle = element_text(face = 'bold', size = 10, color = '#890000'),
#     axis.title.x = element_text(face = 'bold', size = 12, color = '#006400'),
#     axis.title.y = element_text(face = 'bold', size = 12, color = '#006400')
#   )

# this is a linear and non continuous regression model
# Visualising the model for higher resolution and smoother

x_grid <- seq(min(dt$Level), max(dt$Level),0.01)
ggplot()+
  geom_point(aes(dt$Level, dt$Salary),
             shape = 21,
             color = '#890000',
             size = 2.5)+
  geom_line(aes(x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            color = 'darkblue')+
  labs(
    title = 'Decision Tree Regression',
    subtitle = 'by Paul Juverdeanu',
    x = 'Level',
    y = 'Salary'
  )+
  theme_light()+
  theme(
    plot.title = element_text(face = 'bold', size = 14, color = '#006400'),
    plot.subtitle = element_text(face = 'bold', size = 10, color = '#890000'),
    axis.title.x = element_text(face = 'bold', size = 12, color = '#006400'),
    axis.title.y = element_text(face = 'bold', size = 12, color = '#006400')
  )

# it is a powerful model in a more dimensional level










































































































