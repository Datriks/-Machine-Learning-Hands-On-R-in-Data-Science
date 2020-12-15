# Practice-K-Fold Cross Validation Model ----------------------------------
# evaluate the performance of the model
# split the training set in 10 iterations- train the model on 9 iterations and test on the 10th
# this is a model selection
# we are going to use Kernel SVM
# customers are going to click on the social network to buy the SUV car
library(tidyverse)
library(caTools)

dt <- read.csv("Social_network_Ads.csv")
dt <- dt[3:5]

dt

# Encoding the feature as factor ------------------------------------------

dt <- dt %>% 
  mutate(Purchased = factor(Purchased, levels = c(0,1)))
glimpse(dt)

# Splitting the dataset into the training and test set --------------------

set.seed(123)

split <- sample.split(dt$Purchased, SplitRatio = 0.75)
train_set <- subset(dt, split == TRUE)
test_set <- subset(dt, split == FALSE)

glimpse(train_set)
glimpse(test_set)


# Feature Scaling ---------------------------------------------------------

train_set[-3] <- scale(train_set[-3])
test_set[-3] <- scale(test_set[-3])

# Fitting Kernel SVM to the Training set ----------------------------------
library(e1071)

classifier <- svm(formula = Purchased ~.,
                  data = train_set,
                  type = 'C-classification',
                  kernel = 'radial')

# Predicting the Test set results -----------------------------------------

y_pred <- predict(classifier, newdata = test_set[-3])


# Making the Confusion Matrix - Initial evaluation- more empirical --------

library(caret)

cm <- confusionMatrix(y_pred, test_set$Purchased, positive = '1')
cm

# More advanced Evaluation Model- Applying K-Fold Cross Validation --------
# create the 10 folds that divide the training set
folds <- createFolds(train_set$Purchased, k = 10)

# implement the algorithm itself
cv <- lapply(folds, function(x) {
# create each train fold 
  training_fold = train_set[-x, ]
# create each test fold
  test_fold = train_set[x, ]
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
  
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(accuracy)
})
# present each fold accuracy
cv
# our final accuracy is going to be the average of the 10 k fold cross validation of our model
accuracy_final <- mean(as.numeric(cv))
accuracy_final

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


























