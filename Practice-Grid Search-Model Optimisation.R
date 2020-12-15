# Practice-Grid Search-Model Optimisation ---------------------------------
# https://topepo.github.io/caret/available-models.html
# look for Support Vector Machines with Radial Basis Function Kernel
# the grid search answers the question which parameters to choose for the hyperparameters
# Importing the data set
library(tidyverse)
library(caTools)
library(caret)
library(e1071)

dt <- read.csv("Social_Network_Ads.csv")
glimpse(dt)

# Cleaning the data set
dt <- dt[3:5]

#Encoding the target feature as factor
dt$Purchased <- factor(dt$Purchased, levels = c(0,1))

# Splitting the data set into Training and test set

set.seed(123)
split <- sample.split(dt$Purchased, SplitRatio = 0.75)
training <- subset(dt, split == T)
test <- subset(dt, split == F)

# Scaling the two data sets
training[-3] <- scale(training[-3])
test[-3] <- scale(test[-3])

glimpse(training)

# Fitting kernel Radial to the Training set

classifier <- svm(formula = Purchased ~.,
                  data = training,
                  type = 'C-classification',
                  kernel = 'radial')

# Predicting the test results

y_pred <- predict(classifier, newdata = test[-3])

# Create the confusion Matrix
# verify details of simple confusion matrix without optimisation
cm1 <- confusionMatrix(y_pred, test$Purchased, positive = '1')
cm1

# this is confusion matrix in a tabular form that can be evaluated further
cm <- table(test[ ,3], y_pred)
cm

# Applying K-fold Cross Validation
# create the 10 folds that divide the training set
folds <- createFolds(training$Purchased, k = 10)

# implement the algorithm itself
cross_valid <- lapply(folds, function(x) {
  # create each train fold 
  training_fold = training[-x, ]
  # create each test fold
  test_fold = training[x, ]
  # apply the classifier svm to each train fold
  classifier <- svm(formula = Purchased ~.,
                    data = training_fold,
                    type = 'C-classification',
                    kernel = 'radial')
  # creating the predicting test value for each test fold
  y_pred = predict(classifier, newdata = test_fold[-3])
  # compute the confusion Matrix
  cm = table(test_fold[,3], y_pred)
  # compute the accuracies of the 10 folds
# confussion matrix complete evaluation   
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(accuracy)
})
# present each fold accuracy
cross_valid
# our final accuracy is going to be the average of the 10 k fold cross validation of our model
accuracy_final <- mean(as.numeric(cross_valid))
accuracy_final

# use the caret package
# Applying the Grid Search to find the best Parameters
# library(caret) for all machine learning models
# obtaining the optimal values for the model 

classifier <- train(form = Purchased ~., 
                    data = training,
                    method = 'svmRadial')

classifier
# The maximum accuracy with this model we can obtain by choosing 
#sigma = 1.433809 and C = 1
classifier$bestTune


set <- training
x1 <- seq(min(set[,1]) - 1, max(set[,1]) + 1, by = 0.01)
x2 <- seq(min(set[,1]) - 1, max(set[,1]) + 1, by = 0.01)

grid_set <- expand.grid(x1, x2)
colnames(grid_set) <- c('Age', 'EstimatedSalary')

y_grid <- predict(classifier, newdata = grid_set)

plot(set[, -3],
     main = 'SVM (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(x1), ylim = range(x2))

contour(x1, x2, matrix(as.numeric(y_grid), length(x1), length(x2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Visualising the Test set results ----------------------------------------

set <- test
x1 <- seq(min(set[,1]) - 1, max(set[,1]) + 1, by = 0.01)
x2 <- seq(min(set[,1]) - 1, max(set[,1]) + 1, by = 0.01)

grid <- expand.grid(x1, x2)
colnames(grid) <- c('Age', 'EstimatedSalary')

y_grid <- predict(classifier, newdata = grid)

plot(set[, -3],
     main = 'SVM (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(x1), ylim = range(x2))

contour(x1, x2, matrix(as.numeric(y_grid), length(x1), length(x2)), add = TRUE)
points(grid, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))



















































































































































































