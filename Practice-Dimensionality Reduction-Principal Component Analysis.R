# Practice-Dimensionality Reduction-Principal Component Analysis ----------
# Feature Extraction: PCA, LDA, KERNEL PCA.
# from m indep var of your dataset PCA extracts a new independent var 
# that explain the most the variance of the dataset, regardless of the 
# dependent variable.
# the fact that dependent var is not considered makes this model an unsupervised model

# Importing the data set
# customer segment is the dependent variable
# each type of wine corresponds to one type of customer, we have 3 types
# we need to apply dimensionality reduction in order to obtain 2 var that 
# explain the most of the variance, find the prediction regions and prediction boundaries


library(tidyverse)

dt <- read.csv('Wine.csv')
glimpse(dt)

dt %>% 
  view()

# Splitting the data set into training and test set
library(caTools)

set.seed(123)
split <- sample.split(dt$Customer_Segment, SplitRatio = 0.8)
training <- subset(dt, split == T)
test <- subset(dt, split == F)

training %>% 
  view()
test %>% 
  view()

# Feature Scaling

training[-14] <- scale(training[-14])
test[-14] <- scale(test[-14])
training %>% 
  view()

# Create the PCA function
library(caret)
library(e1071)

pca <- preProcess(x = training[-14], 
                  method = 'pca', 
                  pcaComp = 2)
# apply the pca to the training set to obtain the 2 new features
training <- predict(pca, training)

training <- training %>% 
  relocate(Customer_Segment, .after = last_col()) %>% 
  view()

# apply PCA to the test set and rearrange the order of the features
test <- predict(pca, test)

test <- test %>% 
  relocate(Customer_Segment, .after = last_col())

# Fitting the Classification Model to the test set
# from this point we can create any classification model like:
# Logistic Regression, K-NN, SVM, Kernel SVM, Naive Bayes, Decision Tree, 
# Random Forest Classification.

# Fitting the SVM to the Training set

classifier <- svm(formula = Customer_Segment ~.,
                  data = training,
                  type = 'C-classification',
                  kernel = 'linear')

# Predicting the test set results

y_pred <- predict(classifier, newdata = test[-3])

# Building the confusion matrix

cm <- confusionMatrix(data = factor(y_pred, levels = c(1,2,3)),
                      reference = factor(test$Customer_Segment, levels = c(1,2,3)),
                      positive = "1")
cm

# Visualising the Training set results---------------------------------------
# install.packages('ElemStatLearn')
library(ElemStatLearn)

set <- training
x1 <- seq(min(set[,1])-1, max(set[,1])+1, by = 0.01)
x2 <- seq(min(set[,2])-1, max(set[,2])+1, by = 0.01)

grid_set <- expand.grid(x1,x2)
colnames(grid_set) <- c('PC1', 'PC2')
y_grid <- predict(classifier, newdata = grid_set)

plot(set[,-3],
     main = 'SVM using PCA ( Training Set)',
     xlab = 'PC1',
     ylab = 'PC2',
     xlim = range(x1), ylim = range(x2))

contour(x1,x2, matrix(as.numeric(y_grid), length(x1), length(x2)), add = T)
# color of the points and background
points(grid_set, pch = '.', col = ifelse(y_grid == 2, 'deepskyblue', 
                                         ifelse(y_grid == 1, 'springgreen3', 'tomato')))
points(set, pch = 21, bg = ifelse(set[, 3] == 2, 'blue3',
                                  ifelse(set[, 3] == 1, 'green4', 'red3')))

# linear classifier determines the straight line that separates the 2 fields
# the line is called the prediction boundary
# this is a linear classifier
# the regions wont change because they are learned from the training data set
# the regions is what our model predicts
# the points are the real points given in the data set
# green is number 2

# Visualising the test set results

set <- test
x1 <- seq(min(set[,1])-1, max(set[,1])+1, by = 0.01)
x2 <- seq(min(set[,2])-1, max(set[,2])+1, by = 0.01)

grid_set <- expand.grid(x1,x2)
colnames(grid_set) <- c('PC1', 'PC2')
y_grid <- predict(classifier, newdata = grid_set)

plot(set[,-3],
     main = 'SVM using PCA ( Test Set)',
     xlab = 'PC1',
     ylab = 'PC2',
     xlim = range(x1), ylim = range(x2))

contour(x1,x2, matrix(as.numeric(y_grid), length(x1), length(x2)), add = T)
# color of the points and background
points(grid_set, pch = '.', col = ifelse(y_grid == 2, 'deepskyblue', 
                                         ifelse(y_grid == 1, 'springgreen3', 'tomato')))
# represents the area predicted by the model
points(set, pch = 21, bg = ifelse(set[, 3] == 2, 'blue3',
                                  ifelse(set[, 3] == 1, 'green4', 'red3')))










































































































