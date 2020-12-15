install.packages("caTools")
library(caTools)
library(tidyverse)

dataset <- read_csv("Part 1 - Data Preprocessing/Section 2 -------------------- Part 1 - Data Preprocessing --------------------/R/Data.csv")
View(dataset)
dataset

# check the number of nulls
dataset %>% 
  summarise_all(~ sum(is.na(.)))

# sample.split, subset where split ==true, subset where split ==false

# Other way of spliting the data set --------------------------------------
# first create a new column called id

ds7 <- ds6 %>% 
  mutate(
    id = 1:nrow(.)
  )
ds7

training <- ds7 %>% 
  sample_frac(0.8)
training

test <- ds7 %>% 
  anti_join(training,by = "id")
test

#Purchased is the dependent value
set.seed(123)
split <- sample.split(ds6$Purchased, SplitRatio = 0.8)
split
# true is the training set and false to the test set

training_set <- subset(ds6, split == TRUE) # training set
training_set

test_set <- subset(ds6,split == FALSE)
test_set
#----------------------------------------
set.seed(1234)
split1 <- sample.split(ds6$Purchased, SplitRatio = 0.8)
split1

training_set1 <- subset(ds6, split == TRUE)
training_set1

test_set1 <- subset(ds6, split == FALSE)
test_set1

# training and Test Data on Mtcars ----------------------------------------

mtcars

mtcars1 <- mtcars %>% 
  mutate(
    id = 1:nrow(mtcars)
  )
mtcars1

train <- mtcars1 %>% 
  sample_frac(.8)

train

test <- mtcars1 %>% 
  anti_join(train, by = 'id')
test

# -------------------------------------------------------------------------

chick1 <- ChickWeight %>% 
  mutate(
    id = 1:nrow(.),
  )
head(chick1)

# feature Scaling ---------------------------------------------------------
#we have to think about Euclidean distance, 
#the features size needs to be compatible in size



























