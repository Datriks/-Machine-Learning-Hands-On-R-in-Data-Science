# K Nearest Neighbor Classification ----------------------------------------
library(tidyverse)
library(caTools)
library(ElemStatLearn)
library(class)

# Import the data set ------------------------------------------------------

dt <- read_csv('Social_Network_Ads.csv')
dt
glimpse(dt)

dt <- dt[,3:5]
dt

# Encoding the target feature as factor -----------------------------------

dt$Purchased <- factor(dt$Purchased,
                       levels = c(0, 1))
dt
# Spit the data set -------------------------------------------------------

set.seed(123)
split <- sample.split(dt$Purchased, SplitRatio = 0.75)
training <- subset(dt, split == T)
test <- subset(dt, split == F)
training
test

# Scaling the data set ----------------------------------------------------

training[,-3] <- scale(training[,-3])
test[,-3] <- scale(test[,-3])
training
test

# Fitting the classifier K-NN to the training set ------------------------------
# in K-NN classification obtaining the classifier and the prediction in one step
y_pred <- knn(training[,-3], 
              test[,-3],
              cl = training$Purchased,
              k = 5)
y_pred

# Predicting the Test set results -----------------------------------------

#y_pred <- predict(classifier, newdata = test[-3])


# Making the confusion Matrix ---------------------------------------------

cm <- table(test$Purchased, y_pred)
cm

# Visualising the Training set results ------------------------------------

set = training
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)

colnames(grid_set) = c('Age', 'EstimatedSalary')

y_grid <- knn(training[,-3], 
              grid_set,
              cl = training$Purchased,
              k = 5)

plot(set[, -3], main = 'K-NN (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))


# Visualising the Test set results ----------------------------------------

set = test
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)

grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')

y_grid <- knn(training[,-3], 
              grid_set,
              cl = training$Purchased,
              k = 5)

plot(set[, -3], main = 'K-NN (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

























































































