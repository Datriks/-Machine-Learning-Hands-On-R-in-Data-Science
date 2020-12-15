library(tidyverse)
# Practice - Artificial Neural Network-Machine Learning -------------------
# we have to predict classes-yes and no variable1 or 0, this is a classification problem
# use classification model template
# dataset is formed of customers of a bank and we would like to model how ca we predict who # is going to stay with the bank


dt <- read_csv('Churn_Modelling.csv')

# 1 customer left the bank , 0 stayed with the bank
#Exited is the dependent variable
# we are going to select only the variables that could have an impact on the dependent var.
dt <- dt[4:14]
dt

glimpse(dt)


# we do not need to encode the target feature because the package we are going to use will reciognise easily # this encoding 

#dt$Exited <- factor(dt$Exited, levels = c(0,1))

# categorical var. that we have to encode are: geography and gender
# we have to convert them as factors and set them as numeric
# the package that we are going to use for this project require this to be numeric, numeric factors
dt$Geography <- as.numeric(factor(dt$Geography,
                                  levels = c('France','Spain','Germany'),
                                  labels = c(1,2,3)))

dt$Gender <- as.numeric(factor(dt$Gender,
                               levels = c('Female','Male'),
                               labels = c(1,2)))
dt
glimpse(dt)


library(caTools)
set.seed(123)
split <- sample.split(dt$Exited, SplitRatio = 0.8)
training <- subset(dt, split == T)
test <- subset(dt, split == F)

glimpse(training)
glimpse(test)

training[-11] <- scale(training[-11])
test[-11] <- scale(test[-11])

training

library(h2o)
Sys.unsetenv("http_proxy")
# we  have 2 hidden layers
# use H2O is like connecting to a machine with many GPUs
# high efficiency, contain tuning parameter
# we have to establish a connection with the h2o platform which is done in the following way
# the value -1 will take all the nr of cores available for calculation
# download the latest SE jdk : https://www.oracle.com/technetwork/java/javase/downloads/index.html

h2o.init(nthreads = -1)

#h2o.init(ip="localhost", port=54323)

# the number of hidden layer is choosen based on average of nr of ind # variables
classifier <- h2o.deeplearning(y = 'Exited', 
                               training_frame = as.h2o(training),
                               activation = 'Rectifier',
                               hidden = c(6,6),
                               epochs = 100,
                               train_samples_per_iteration = -2)

# prediction of the probability that the customers left the bank =1
prob_pred <- h2o.predict(classifier, newdata = as.h2o(test[-11]))
# transform the probabilities back into 1 and 0
# we choose a probability threshold of 0.5 in medical choose a higher threshold of 0.8
y_pred <- (prob_pred > 0.5)
# this is still in a h20 object
# convert that h2o object into a vector
y_pred <- as.vector(y_pred)
y_pred


# cm <- table(test$Exited, y_pred)
# cm

library(caret)

# cm <- confusionMatrix(
#   factor(y_pred, levels = c(0,1)),
#   factor(test$Exited, levels = c(0,1)))
# 
# cm 


cm <- confusionMatrix(data = factor(y_pred, levels = c(0,1)),
                      reference = factor(test$Exited, levels = c(0,1)),
                      positive = "1")
cm


h2o.shutdown()
Y




































































































