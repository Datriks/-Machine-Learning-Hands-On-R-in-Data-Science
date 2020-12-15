# Importing the data set --------------------------------------------------
library(tidyverse)
library(caTools)
library(e1071)
library(ElemStatLearn)

dt <- read_csv("Social_Network_Ads.csv")
glimpse(dt)
dt

dt <- dt[,3:5]
dt

# Encoding the target feature as factor -----------------------------------

dt$Purchased <- factor(dt$Purchased,
                       levels = c(0,1))
dt
# Splitting the data set into training and test ----------------------------
set.seed(123)
split = sample.split(dt$Purchased, SplitRatio = 0.75)
training = subset(dt, split == TRUE)
test = subset(dt, split == FALSE)
training
test

# Feature Scaling ---------------------------------------------------------

training[-3] <- scale(training[-3])
test[-3] <- scale(test[-3])
training
test

# Fitting classifier to the training set ----------------------------------
# the data is not linearly separable
# the Gaussian RBF Kernel, sigmoid kernel, polynomial kernel
# RBF radial base function
# non linear kernel 
# here we use library('e1071)
classifier <- svm(formula = Purchased ~ ., 
                  data = training,
                  type = "C-classification",
                  kernel = "radial") # Gaussian Kernel
classifier
# Predicting the test set results -----------------------------------------

y_pred <- predict(classifier, newdata = test[-3])
y_pred

# Construct the confusion matrix -----------------------------------------

cm <- table(test$Purchased, y_pred)
cm

# Visualising the Training set results ------------------------------------
#library(ElemStatLearn)

set <- training
x1 <- seq(min(set[,1]) - 1, max(set[,1]) + 1, by = 0.01)
x2 <- seq(min(set[,1]) - 1, max(set[,1]) + 1, by = 0.01)

grid_set <- expand.grid(x1, x2)
colnames(grid_set) <- c('Age', 'EstimatedSalary')

y_grid <- predict(classifier, newdata = grid_set)

plot(set[, -3],
     main = 'Kernel SVM (Training set)',
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
     main = 'Kernel SVM (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(x1), ylim = range(x2))

contour(x1, x2, matrix(as.numeric(y_grid), length(x1), length(x2)), add = TRUE)
points(grid, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))










































































































