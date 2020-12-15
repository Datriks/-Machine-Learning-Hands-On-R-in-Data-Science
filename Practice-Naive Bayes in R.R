# Practice-Naive Bayes in R -----------------------------------------------
library(tidyverse)
library(caTools)
library(ElemStatLearn)
library(e1071)

# Read data- Import data --------------------------------------------------

dt <- read_csv("Social_Network_Ads.csv")
dt

glimpse(dt)
dt <- dt[3:5]
dt


# Element Encoding - Factor -----------------------------------------------

dt[3] <- factor(dt$Purchased, levels = c(0,1))
dt


# Split The data set ------------------------------------------------------

set.seed(123)
split <- sample.split(dt$Purchased, SplitRatio = 0.75)
training <- subset(dt, split == T)
test <- subset(dt, split == F)
training
test

# Feature Scaling --------------------------------------------------------

training[-3] <- scale(training[-3])
test[-3] <- scale(test[-3])
training
test

# Fitting the naive Bayes classifier to the training set --------------------------
# library(e1071)

classifier <- naiveBayes(x = training[-3], 
                         y = training$Purchased)

classifier
# Predicting the values of the test set -----------------------------------

y_pred <- predict(classifier, newdata = test[-3])
y_pred
length(y_pred)
length(test$Purchased)

# Building Confusion Matrix -----------------------------------------------

cm <- table(test$Purchased, y_pred)
cm

# Visualising the Training set results
#library(ElemStatLearn)

set = training
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)

grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')

y_grid = predict(classifier, newdata = grid_set)

plot(set[, -3],
     main = 'Naive Bayes (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))

contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Visualising the Test set results
#library(ElemStatLearn)
set = test
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)

grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')

y_grid = predict(classifier, newdata = grid_set)

plot(set[, -3], main = 'Naive Bayes (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))

contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))










































































