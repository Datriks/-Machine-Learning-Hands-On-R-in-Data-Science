# Support Vector Regression -----------------------------------------------
install.packages('e1071')

library(tidyverse)
library(e1071)   # used for svm- svr to calculate the regressor
library(caTools)

dataset <- read_csv('Position_Salaries.csv')
dataset

dataset %>% 
  summarise_all(~ sum(is.na(.)))

dataset <- dataset[,2:3]
dataset

# Splitting the dataset
# set.seed(123)
# split <- sample.split(dataset$Salary, SplitRatio = 0.8)
# training_set <- subset(dataset, split == TRUE)
# training_set
# test_set <- subset(dataset, split == FALSE)
# test_set

# Feature Scaling ---------------------------------------------------------

# training_set <- scale(training_set)
# test_set <- scale(test_set)

# Fitting the SVR to the Training set -------------------------------------

regressor <- svm(formula = Salary ~ .,
                data = dataset,
                type = 'eps-regression')

?svm
regressor


# Predicting a new result -------------------------------------------------

y_pred <- predict(regressor,data.frame(Level = 6.5))
y_pred

# Visualising the SVR Result ----------------------------------------------

ggplot()+
  geom_point(aes(x = dataset$Level, y = dataset$Salary), 
             color = '#006400',
             shape = 21, size = 2)+
  geom_line(aes(x = dataset$Level, y = predict(regressor, newdata = dataset)),
            color = '#890000',
            size = 0.8,
            linetype = "dotted")+
  labs(
    title = 'SVR Classification',
    subtitle = 'by Paul Juverdeanu',
    x = 'Level',
    y = 'Salary'
  )+
  theme_light()+
  theme(
    plot.title = element_text(face = 'bold', size = 14, color = '#006400'),
    plot.subtitle = element_text(face = 'italic',size = 10, color = '#006400'),
    axis.title.x = element_text(face = 'bold',size = 10,color = '#006400'),
    axis.title.y = element_text(face = 'bold',size = 10, color = '#006400')
  )
















































































