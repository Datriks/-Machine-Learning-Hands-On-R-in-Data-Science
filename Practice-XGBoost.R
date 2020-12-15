# Practice-XGBoost --------------------------------------------------------
# Importing the data set
# exteme gradient boosting
# we are gonna use ANN-artificial neural networks - classification problem
# #XGBoost is an algorithm that has recently been dominating applied 
# machine learning and Kaggle competitions for structured or tabular data. 
# XGBoost is an implementation of gradient boosted decision trees designed for 
# speed and performance
library(tidyverse)

dt <- read.csv("Churn_Modelling.csv")
dt <- dt[4:14]
glimpse(dt)

# Encoding the categorical variables

dt$Geography <- as.numeric(factor(dt$Geography, 
                       levels = c('France', 'Spain','Germany'),
                       labels = c(1,2,3)))

dt$Gender <- as.numeric(factor(dt$Gender, 
                       levels = c('Female', 'Male'),
                       labels = c(1,2)))
glimpse(dt)

# Splitting the data set into training and test data set
library(caTools)

set.seed(123)
split <- sample.split(dt$Exited, SplitRatio = 0.8)
train_set <- subset(dt, split == T)
test_set <- subset(dt, split == F)

glimpse(test_set)

# Feature Scaling
# nit necessary in XGBoost we can keep the interpretation of our dataset
# train_set[-11] <- scale(train_set[-11])
# test_set[-11] <- scale(test_set[-11])

# Fitting XGBoost to the Training set
# install.packages("xgboost")
library(xgboost)
# only the features are imported without the dependent var
classifier <- xgboost(data = as.matrix(train_set[-11]),
                      label = train_set$Exited,
                      nrounds = 10)

# Applying K-fold Cross Validation
# create the 10 folds that divide the training set
library(caret)
folds <- createFolds(train_set$Exited, k = 10)

# implement the algorithm itself
cross_valid <- lapply(folds, function(x) {
  # create each train fold 
  training_fold = train_set[-x, ]
  # create each test fold
  test_fold = train_set[x, ]
  # apply the classifier svm to each train fold
  classifier <- xgboost(data = as.matrix(train_set[-11]),
                        label = train_set$Exited,
                        nrounds = 10)
  # creating the predicting test value for each test fold
  y_pred = predict(classifier, newdata = as.matrix(test_fold[-11]))
  y_pred = (y_pred >= 0.5)
  # compute the confusion Matrix
  cm = table(test_fold[,11], y_pred)
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
































































































