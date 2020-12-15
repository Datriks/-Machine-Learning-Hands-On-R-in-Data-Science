library(tidyverse)
library(caTools)
library(e1071)
library(randomForest)
library(ElemStatLearn)

# Impost the data set -----------------------------------------------------

dt <- read_csv("Social_Network_Ads.csv")
glimpse(dt)
dt

# Encode the dependent variable -------------------------------------------

dt <- dt %>% 
  select(
    Age:Purchased
  ) %>% 
  mutate(
    Purchased = factor(Purchased, levels = c(0,1))
  )
dt

# Split the data set ------------------------------------------------------

set.seed(123)
split <- sample.split(dt$Purchased, SplitRatio = 0.75)
training <- subset(dt, split == T)
test <- subset(dt, split == F)
training
test

# Standardize the data sets -----------------------------------------------

training[-3] <- scale(training[-3])
test[-3] <- scale(test[-3])
training
test

# Fitting Classifier to the training set ----------------------------------

classifier <- randomForest(x = training[-3], 
                           y = training$Purchased,
                           ntree = 500)

# Predicting the test set results -----------------------------------------

y_pred <- predict(classifier, newdata = test[-3])

# Create the confusion matrix ---------------------------------------------

cm <- table(test$Purchased, y_pred)
cm

# Visualising the Training set results

set = training
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)

grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')

y_grid = predict(classifier, newdata = grid_set)

plot(set[, -3],
     main = 'Random Forest (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Visualising the Test set results

set = test
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)

colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)

plot(set[, -3], main = 'Random Forest (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))









































































