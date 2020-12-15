
# Practice-Linear Discriminant Analysis-LDA-DimReduction ------------------

# used for dimensionality reduction
# used in the pre-processing step for pattern classification
# has the goal to project the dataset to a lower dimensional space
# project n dimensional samples to a lower subspace k<=n-1 while maintaining 
# the discriminatory information
# both PCA and LDA are linear techniques used for dimensional reduction
# PCA is unsupervised while LDA is supervised because of the relation to the dependent variable.
# PCA component axes that maximise the variance
#LDA maximising the component axes for class separation

# Import the necessary packages
library(tidyverse)
library(caTools)
library(e1071)
library(caret)
library(ElemStatLearn)

# Import the data set
dt <- read_csv("Wine.csv")
glimpse(dt)

# Split the data set
set.seed(123)
split <- sample.split(dt$Customer_Segment, SplitRatio = 0.8)
training <- subset(dt, split == T)
test <- subset(dt, split == F)

# Feature Scaling

training[-14] <- scale(training[-14])
test[-14] <- scale(test[-14])

# Create the LDA function
library(MASS)
# it is a supervised linear reduction model that takes in consideration the dependent var
# lda object created
lda <- lda(formula = Customer_Segment ~.,
           data = training)
# the new training set composed only of the 2 new variables
# with predict in lda case we obtain a matrix and we need a data frame
# class column is the dependent variable
training <- as.data.frame(predict(lda, training))
training <- training[,c(5,6,1)]

test <- as.data.frame(predict(lda, test))
test <- test[,c(5,6,1)]

# Fitting the LDA classification model to the Training set
# from this point we can create any classification model like:
# Logistic Regression, K-NN, SVM, Kernel SVM, Naive Bayes, Decision Tree, 
# Random Forest Classification.
# library (e1071)

classifier <- svm(formula = class ~.,
                  data = training,
                  type = 'C-classification',
                  kernel = 'linear')
# Predict the test set results

y_pred <- predict(classifier, newdata = test[-3])

# Building the confusion Matrix

cm <- confusionMatrix(data = factor(y_pred, levels = c(1,2,3)),
                      reference = factor(test[, 3], levels = c(1,2,3)),
                      positive = "1")
cm

# Visualising the Training set results---------------------------------------
# install.packages('ElemStatLearn')
#library(ElemStatLearn)

set <- training
x1 <- seq(min(set[,1])-1, max(set[,1])+1, by = 0.01)
x2 <- seq(min(set[,2])-1, max(set[,2])+1, by = 0.01)

grid_set <- expand.grid(x1,x2)
colnames(grid_set) <- c('x.LD1', 'x.LD2')
y_grid <- predict(classifier, newdata = grid_set)

plot(set[,-3],
     main = 'SVM using LDA ( Training Set)',
     xlab = 'LD1',
     ylab = 'LD2',
     xlim = range(x1), ylim = range(x2))

contour(x1,x2, matrix(as.numeric(y_grid), length(x1), length(x2)), add = T)
# color of the points and background
points(grid_set, pch = '.', col = ifelse(y_grid == 2, 'deepskyblue', 
                                         ifelse(y_grid == 1, 'springgreen3', 'tomato')))
points(set, pch = 21, bg = ifelse(set[, 3] == 2, 'blue3',
                                  ifelse(set[, 3] == 1, 'green4', 'red3')))

# Visualising the test set results

set <- test
x1 <- seq(min(set[,1])-1, max(set[,1])+1, by = 0.01)
x2 <- seq(min(set[,2])-1, max(set[,2])+1, by = 0.01)

grid_set <- expand.grid(x1,x2)
colnames(grid_set) <- c('x.LD1', 'x.LD2')
y_grid <- predict(classifier, newdata = grid_set)

plot(set[,-3],
     main = 'SVM using LDA ( Test Set)',
     xlab = 'LD1',
     ylab = 'LD2',
     xlim = range(x1), ylim = range(x2))

contour(x1,x2, matrix(as.numeric(y_grid), length(x1), length(x2)), add = T)
# color of the points and background
points(grid_set, pch = '.', col = ifelse(y_grid == 2, 'deepskyblue', 
                                         ifelse(y_grid == 1, 'springgreen3', 'tomato')))
# color of the area 
points(set, pch = 21, bg = ifelse(set[, 3] == 2, 'blue3',
                                  ifelse(set[, 3] == 1, 'green4', 'red3')))






































































































