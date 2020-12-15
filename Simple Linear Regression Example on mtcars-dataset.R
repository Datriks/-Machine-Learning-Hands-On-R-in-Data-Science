library(tidyverse)
library(caTools)

#By Pul Juverdeanu

rm(list = ls())

mtcars
glimpse(mtcars)

mtcars %>% 
  summarise_all(~ sum(is.na(.)))

mtcars1 <- mtcars %>% 
  select(
    wt,qsec
  )
mtcars1

# Encoding the factor variable
# dataset$Country = factor(dataset$Country,
#                          levels = c('France', 'Spain', 'Germany'),
#                           labels = c(1, 2, 3))

# split the data set
set.seed(123)
split <- sample.split(mtcars1$qsec, SplitRatio = 2/3)

training_set <- subset(mtcars1, split == TRUE)
training_set

test_set <- subset(mtcars1,split == FALSE)
test_set

# scalling

training_set[,1:2] <- scale(training_set[,1:2])
training_set

test_set[,1:2] <- scale(test_set[,1:2])
test_set

# Creating the model

regressor <- lm(formula = qsec ~ wt, data = training_set)
regressor

summary(regressor)

# Predicting the test results

y_predict <- predict(regressor, newdata = test_set)
y_predict

# visualising the regression for the test set

ggplot()+
  geom_point(aes(training_set$wt, training_set$qsec), 
             color = 'red',
             shape =21)+
  geom_line(aes(training_set$wt, y = predict(regressor, newdata = training_set)),
            color = 'darkblue',
            size = 0.8)+
  labs(title = "qsec vs wt (Training Set)",
       subtitle = "by Juverdeanu",
       x = "wt",
       y = "qsec")+
  theme_light()


ggplot()+
  geom_point(aes(test_set$wt, test_set$qsec), 
             color = 'red',
             shape =21)+
  geom_line(aes(training_set$wt, y = predict(regressor, newdata = training_set)),
            color = 'darkblue',
            size = 0.8)+
  labs(title = "qsec vs wt (Test Set)",
       subtitle = "by Juverdeanu",
       x = "wt",
       y = "qsec")+
  theme_light()





































































