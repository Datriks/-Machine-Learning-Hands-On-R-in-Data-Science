install.packages("randomForest")

library(tidyverse)
library(caTools)
library(e1071)
library(rpart)
library(randomForest)


dt <- read_csv("Position_Salaries.csv")

glimpse(dt)

dt %>% summarize_all(~ sum(is.na(.)))
dt <- dt %>% 
  select(2:3)
dt

# Splitting the data set

# set.seed(123)
# split <- sample.split(dt$Salary, SplitRatio = 0.8)
# training <- subset(dt, split == T)
# test <- subset(dt, split == F)

# Scale the data set (normalisation)

# training <- scale(training)
# test <- scale(test)

# Fitting the Random Forest Regression to the data set
set.seed(1234)
regressor <- randomForest(x = dt[1], # this gives us a data frame
                          y = dt$Salary,
                          ntree = 500) # this gives us a vector, y expects a vector
regressor
summary(regressor)

# Predicting the new result
  
y_predict <- predict(regressor, data.frame(Level = 6.5))
y_predict

# Visualising the Random Forest Regression results ( for higher resolution and )

x_grid <- seq(min(dt$Level), max(dt$Level), 0.01)
ggplot()+
  geom_point(aes(x = dt$Level, y = dt$Salary),
             shape = 21,
             color = '#890000',
             size =2.5)+
  geom_line(aes(x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            color = '#002949',
            #linetype = 'dotted',
            size = 0.8)+
  theme_light()+
  labs(
    title = 'Random Forest Regression',
    subtitle = 'by Paul Juverdeanu',
    x = 'Level',
    y = 'Salary'
  )+
  theme(
    plot.title = element_text(face = 'bold', size = 14, color = '#006400'),
    plot.subtitle = element_text(face = 'bold', size = 10, color = '#890000'),
    axis.title.x = element_text(face = 'bold', size = 12, color = '#006400'),
    axis.title.y = element_text(face = 'bold', size = 12, color = '#006400')
  )
































































































