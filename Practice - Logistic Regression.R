# Logistic Regression -----------------------------------------------------
# get the package from this address:
# https://cran.r-project.org/src/contrib/Archive/ElemStatLearn/
# Download the package from this link or use the link above.
# You can then go to R-Studio. Please proceed to the tools menu and click install packages.
# Under 'Install from:' choose package install file(.zip; .tar,etc)
# Choose the downloaded package

install.packages('ElemStatLearn')
library(ElemStatLearn)

library(tidyverse)
library(caTools)

# Import data -------------------------------------------------------------

dt <- read_csv("Social_Network_Ads.csv")

glimpse(dt)
dt

# Select the indexes that we are goina use for our model -------------------

dt <- dt %>% 
  select(Age:Purchased)
dt
# Splitting the data set --------------------------------------------------

set.seed(123)
split <- sample.split(dt$Purchased, SplitRatio = 0.75)
training <- subset(dt, split == T)
training
test <- subset(dt, split == F)
test

# Feature Scaling --------------------------------------------------------

training[,1:2] <- scale(training[,1:2])
test[,1:2] <- scale(test[,1:2])
training
test
# Fitting the Logistic Regression to the data set -----------------------
# glm - generalised linear model

classifier <- glm(formula = Purchased ~ .,
                  family = binomial,
                  data = training)
classifier
summary(classifier)

# Predicting the Test set results -----------------------------------------

prob_pred <- predict(classifier, type = 'response', newdata = test[-3])
prob_pred

# create a vector of our predicted results
# we consider the limit 50% any values under 0.5 will be equalised to 0 (not bought) and
# any values above 0.5 will be considered 1(bought)
y_pred <- ifelse(prob_pred > 0.5, 1, 0)
y_pred

# Building the Confusion Matrix --------------------------------------------
# is going to count the correct predictions and incorrect predictions

cm <- table(test$Purchased, y_pred)
cm

# Visualising the Training set results---------------------------------------
# install.packages('ElemStatLearn')
# library(ElemStatLearn)

set <- training
x1 <- seq(min(set[,1])-1, max(set[,1])+1, by = 0.01)
x2 <- seq(min(set[,2])-1, max(set[,2])+1, by = 0.01)

grid_set <- expand.grid(x1,x2)
colnames(grid_set) <- c('Age', 'EstimatedSalary')
grid_set

prob_set <- predict(classifier,type = 'response', newdata = grid_set)
y_grid <- ifelse(prob_set > 0.5, 1, 0)

plot(set[,-3],
     main = 'Logistic Regression ( Training Set)',
     xlab = 'Age',
     ylab = 'Estimated Salary',
     xlim = range(x1), ylim = range(x2))

contour(x1,x2, matrix(as.numeric(y_grid), length(x1), length(x2)), add = T)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# linear classifier determines the straight line that separates the 2 fields
# the line is called the prediction boundary
# red region is 0, people will not buy the SUV
# this is a linear classifier
# the regions wont change because they are learned from the training data set


# Visualising the Test set results ----------------------------------------
#select the data set
set <- test
x1 <- seq(min(set[,1])-1, max(set[,1])+1, by = 0.01)
x2 <- seq(min(set[,2])-1, max(set[,2])+1, by = 0.01)
#create a grid (matrix) for the Age and EstimatedSalary
grid_set <- expand.grid(x1,x2)
colnames(grid_set) <- c('Age', 'EstimatedSalary')
# calculate the prediction
prob_set <- predict(classifier,type = 'response', newdata = grid_set)
y_grid <- ifelse(prob_set > 0.5, 1, 0)

plot(set[,-3],
     main = 'Logistic Regression ( Test Set)',
     xlab = 'Age',
     ylab = 'Estimated Salary',
     xlim = range(x1), ylim = range(x2))

contour(x1,x2, matrix(as.numeric(y_grid), length(x1), length(x2)), add = T)
# color the whole pixels on the grid
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
# color the points 0 who did not buy the SUV and 1 who bought the SUV
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

















































































