
# Multiple Linear regression ----------------------------------------------
# by Paul Juverdeanu
install.packages("extrafont")
extrafont::loadfonts(device="win")
library(tidyverse)
library(caTools)

# data preprocessing
dataset <- read_csv("50_Startups.csv")
head(dataset)

rm(list = ls())

dataset %>% 
  summarise_all(~ sum(is.na(.)))

# profit is the dependent variable the rest are independent variables
# assumptions: linearity,homoscedasticity,multivariate normality,
# Independence of errors, lack of multicolinearity
# dummy variable is the state column is a categorical variable needs to be encoded
# for every state we need to create a column and use 0 and 1;
# the number of dummy variables is equal with the nr of states  minus 1, dummy var trap
# y = b0+b1*x1+b2*x2....b4*D1---D1 being one of the dummy variable


# Encode the categorical variable (State): --------------------------------

dataset1 <- dataset %>% 
  mutate(
    State = factor(State, 
                   levels = c("New York","California","Florida"),
                   labels = c(1,2,3))
  )
dataset1

# Splitting the data set into the training set and test set ---------------

set.seed(123)
split <- sample.split(dataset1$Profit, SplitRatio = 0.8)

training_set <- subset(dataset1, split == TRUE)
training_set

test_set <- subset(dataset1, split == FALSE)
test_set

# Feature Scaling --------------------------------------------------------
#not using in this example

# training_set[,2:3] <- scale(training_set[,2:3])
# training_set
# test_set[,2:3] <- scale(test_set[,2:3])
# test_set

# Fitting Multiple Linear Regression to the training set ------------------

#regressor <- lm(formula = Profit ~ `R&D Spend` + Administration + `Marketing Spend` + State,
                #data = training_set)
# another way to write the formula for the regressor is:
regressor <- lm(formula = Profit ~ . , data = training_set)
regressor
summary(regressor)

#we can rewrite this because the only var with significant influence on the Profit
# is `R&D Spend`:

regressor <- lm(formula = Profit ~ `R&D Spend`, data = training_set)
summary(regressor)

# Predicting the test set results -----------------------------------------

y_predict <- predict(regressor, newdata = test_set)
y_predict


# Visualising the Training set fitting regressor --------------------------

training_set %>% 
  ggplot()+
  geom_point(aes(x = `R&D Spend`, y = Profit, color = State))+
  geom_line(aes(`R&D Spend`, y = predict(regressor, newdata = training_set)),
            color = '#890000',
            size = 0.8)+
  theme_light()

# Building the optimal model using Backward Elimination -------------------
# put all the independent variables and then eliminate one by one those that have a 
# p value too big and are not significant:
# select significance level,fit the model with all the predictors
# consider the predictor with the highest p value, remove that predictor,
# fit the model without this predictor

# first step
regressor <- lm(formula = Profit ~  `R&D Spend` + Administration + `Marketing Spend` + State,
                data = dataset1)
summary(regressor)
# second step - eliminated State variable
regressor <- lm(formula = Profit ~  `R&D Spend` + Administration + `Marketing Spend`,
                data = dataset1)
summary(regressor)
# remove Administration as a predictor and fit the model
regressor <- lm(formula = Profit ~  `R&D Spend` + `Marketing Spend`,
                data = dataset1)
summary(regressor)

regressor <- lm(formula = Profit ~  `R&D Spend`,
                data = dataset1)
summary(regressor)
# if we keep marketing Spend our model is better as reflected by R -squared and
# adjusted R -squared.
# tyhis is the final model:
regressor <- lm(formula = Profit ~  `R&D Spend` + `Marketing Spend`,
                data = training_set)
summary(regressor)

y_predict <- predict(regressor, newdata = test_set)
y_predict

# Visualising the training set ------------------------------------------------

ggplot()+
  geom_point(aes(training_set$`R&D Spend`,training_set$Profit, color = training_set$State))+
  geom_line(aes(training_set$`R&D Spend`, y = predict(regressor, newdata = training_set)),
            color = 'darkblue',
            size=1)+
  labs(
    title = "Training set",
    subtitle = "by P Juverdeanu",
    x = "R&D Spend",
    y = "Profit"
  )+
  theme_light()+
  theme(legend.position = "top",
        legend.title = element_text(color = 'red', 2))+
  scale_colour_discrete("State")

# Visualising the test set ------------------------------------------------

ggplot()+
  geom_point(aes(test_set$`R&D Spend`,test_set$Profit, color = test_set$State))+
  geom_line(aes(training_set$`R&D Spend`, y = predict(regressor, newdata = training_set)),
            color = 'darkblue',
            size=1)+
  labs(
    title = "Test set",
    subtitle = "by P Juverdeanu",
    x = "R&D Spend",
    y = "Profit"
  )+
  theme_light()+
  theme(legend.position = "top",
        legend.title = element_text(color = 'red', 2))+
  scale_colour_discrete("State")

# Automatic Backward Elimination code -------------------------------------

backwardElimination <- function(x, sl) {
  numVars = length(x)
  for (i in c(1:numVars)){
    regressor = lm(formula = Profit ~ ., data = x)
    maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
    if (maxVar > sl){
      j = which(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"] == maxVar)
      x = x[, -j]
    }
    numVars = numVars - 1
  }
  return(summary(regressor))
}

SL = 0.05
dataset = dataset[, c(1,2,3,4,5)]
backwardElimination(training_set, SL)











































































