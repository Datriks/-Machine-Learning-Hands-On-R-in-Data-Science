# Data Preprosessing step -------------------------------------------------
library(tidyverse)
library(caTools)

#By Pul Juverdeanu
dataset <- read_csv("Salary_Data.csv")
View(dataset)

dataset %>% 
  summarise_all(~ sum(is.na(.)))

dataset

# Encoding categorical data

# dataset$Country = factor(dataset$Country,
#                          levels = c('France', 'Spain', 'Germany'),
#                          labels = c(1, 2, 3))
# dataset$Purchased = factor(dataset$Purchased,
#                            levels = c('No', 'Yes'),
#                            labels = c(0, 1))
# correlation between salary and years of experience
# dependent var is salary

# Split the dataset -------------------------------------------------------

dataset <- dataset %>% 
  mutate(
    id = 1:nrow(.)
  )
dataset

training <- dataset %>% 
  sample_frac(0.8)
training <- training %>% 
  select(1:2)
training

test <- dataset %>% 
  anti_join(training, by = "id")

test <- test %>% 
  select(1:2)
test
# ------------------------------------------------------------------------------
# set.seed(123)
# split <- sample.split(dataset$Salary, SplitRatio = 2/3)
# training_set <- subset(dataset, split == TRUE)
# test_set <- subset(dataset, split == FALSE)
# ----------------------------------------------------------------------------
# feature scaling not required for this simple regression

# training_set[,2:3] <- scale(training_set[,2:3])
# training_set
# test_set[,2:3] <- scale(test_set[,2:3])
# test_set

# Fitting Simple Linear Regression to the Training set

regressor <- lm(formula = Salary ~ YearsExperience, 
                data = training)
regressor
summary(regressor)

?lm

# Predicting the test set results -----------------------------------------

y_predict <- predict(regressor, newdata = test)
y_predict
test

# Visualising the Training set results ------------------------------------

training %>% 
  ggplot(aes(YearsExperience, Salary))+
  geom_point(color = 'darkblue')+
  geom_line(aes(YearsExperience, y = predict(regressor, newdata = training)),
            color = 'red', size = 0.8)+
  labs(title = "Salary vs Experience (Training Set)",
       subtitle = "by Juverdeanu",
       x = "Years of Experience",
       y = "Salary")+
  theme_light()

# Apply the regression for the test set

training %>% 
  ggplot(aes(YearsExperience, Salary))+
  geom_point(color = 'darkblue')+
  geom_line(aes(YearsExperience, y = predict(regressor, newdata = training)),
            color = 'red', size = 0.8)+
  labs(title = "Salary vs Experience (Test Set)",
       subtitle = "by Juverdeanu",
       x = "Years of Experience",
       y = "Salary")+
  theme_light()


ggplot()+
  geom_point(aes(test$YearsExperience,test$Salary), color = 'red')+
  geom_line(aes(training$YearsExperience, y = predict(regressor, newdata = training)),
            color = 'blue',
            size = 0.8)+
  labs(title = "Salary vs Experience (Test Set)",
       subtitle = "by Juverdeanu",
       x = "Years of Experience",
       y = "Salary")+
  theme_light()








































































