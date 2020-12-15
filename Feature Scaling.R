# feature Scaling ---------------------------------------------------------
#we have to think about Euclidean distance, 
#the features size needs to be compatible in size
library(tidyverse)
library(caTools)

#Standardisation and normalisation

training_set <- scale(training_set)
test_set <- scale(test_set)

# Error in colMeans(x, na.rm = TRUE) : 'x' must be numeric this is the error
#factors are not a numeric value
# we have to exclude the factors from the feature scaling

training_set[,2:3] <- scale(training_set[,2:3])
training_set
test_set[,2:3] <- scale(test_set[,2:3])
test_set









































































