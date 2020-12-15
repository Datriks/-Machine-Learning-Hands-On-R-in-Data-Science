# Practice-Kernel PCA-Dimensionality Reduction-NonLinear ------------------
# lift to a higher dimension then we extract the principal components or
# independent features

library(tidyverse)
library(e1071)
library(caret)
library(caTools)
library(ElemStatLearn)

dt <- read_csv("Social_Network_Ads.csv")

dt <- dt %>% 
  select(3:5)
glimpse(dt)

# Splitting the data set in training and test set

set.seed(123)
split <- sample.split(dt$Purchased, SplitRatio = 0.75)
training <- subset(dt, split == T)
test <- subset(dt, split == F)

# Feature Scaling

training[,-3] <- scale(training[, -3])

test[,-3] <- scale(test[, -3])

# Creating and Applying Kernel PCA
#install.packages("kernlab")
library(kernlab)

kpca <- kpca(~., 
             data = training[-3],
             kernel = 'rbfdot',
             features = 2)

training_pca <- as.data.frame(predict(kpca, training))
training_pca$Purchased <- training$Purchased 

test_pca <- as.data.frame(predict(kpca, test))
test_pca$Purchased <- test$Purchased

# Fitting Kernel PCA to the training set

classifier <- glm(formula = Purchased ~.,
                  family = binomial,
                  data = training_pca)

# Predict the Test set results

prob_pred <- predict(classifier, type = 'response', newdata = test_pca[-3])

typeof(prob_pred)
class(prob_pred)
attributes(prob_pred)

y_pred <- ifelse(prob_pred > 0.5, 1, 0)

# Making the Confusion Matrix

cm <- confusionMatrix(data = factor(y_pred, levels = c(0, 1)),
                      reference = factor(test_pca$Purchased, levels = c(0, 1)),
                      positive = '1')
cm

# Visualising the Training set results---------------------------------------
# install.packages('ElemStatLearn')
#library(ElemStatLearn)

set <- training_pca
x1 <- seq(min(set[,1])-1, max(set[,1])+1, by = 0.01)
x2 <- seq(min(set[,2])-1, max(set[,2])+1, by = 0.01)

grid_set <- expand.grid(x1,x2)
colnames(grid_set) <- c('V1', 'V2')
prob_set <- predict(classifier, type = 'response', newdata = grid_set)
y_grid <- ifelse(prob_set >0.5, 1, 0)

plot(set[,-3],
     main = 'Logistic Regression using Kernel PCA ( Training Set)',
     xlab = 'V1-Age',
     ylab = 'V2-Estimated Salary',
     xlim = range(x1), ylim = range(x2))

contour(x1,x2, matrix(as.numeric(y_grid), length(x1), length(x2)), add = T)
# color of the points and background
points(grid_set, pch = '.', col = ifelse(y_grid == 2, 'deepskyblue', 
                                         ifelse(y_grid == 1, 'springgreen3', 'tomato')))
points(set, pch = 21, bg = ifelse(set[, 3] == 2, 'blue3',
                                  ifelse(set[, 3] == 1, 'green4', 'red3')))

# Visualising the test set results

set <- test_pca
x1 <- seq(min(set[,1])-1, max(set[,1])+1, by = 0.01)
x2 <- seq(min(set[,2])-1, max(set[,2])+1, by = 0.01)

grid_set <- expand.grid(x1,x2)
colnames(grid_set) <- c('x.LD1', 'x.LD2')
# y_grid must be the same as y_pred
prob_set <- predict(classifier, type = 'response', newdata = grid_set)
y_grid <- ifelse(prob_set >0.5, 1, 0)

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




















































































