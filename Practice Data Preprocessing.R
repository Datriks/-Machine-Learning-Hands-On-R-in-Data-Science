library(tidyverse)

# Import the dataset ------------------------------------------------------
library(readr)
getwd()
dataset <- read_csv("Part 1 - Data Preprocessing/Section 2 -------------------- Part 1 - Data Preprocessing --------------------/R/Data.csv")
View(dataset)
dataset

# Missing Data ------------------------------------------------------------

dataset$Age <- ifelse(is.na(dataset$Age),
                      ave(dataset$Age, FUN = function(x) meanx, na.rm = T))


ds1 <- dataset %>% 
  mutate(
    Age = replace_na(Age, mean(Age, na.rm = T))
  )
ds1

ds2 <- ds1 %>% 
  mutate(
    Salary = replace_na(Salary, mean(Salary, na.rm = T))
  )
ds2
dataset

ds3 <- dataset %>% 
  mutate(
    Age = round(replace_na(Age,mean(Age,na.rm = T)),0),
    Salary = round(replace_na(Salary, mean(Salary, na.rm = T)),0))
ds3

ds4 <- dataset %>% 
  mutate(
    Age = round(ifelse(is.na(Age),mean(Age, na.rm = T),Age),0),
    Salary = round(ifelse(is.na(Salary),mean(Salary, na.rm = T),Salary),0)
  )
ds4

# Encoding the Categorical Data -------------------------------------------

ds5 <- dataset %>% 
  mutate(
    Country = as_factor(Country),
    Purchased = as_factor(Purchased)
  )
ds5

levels(ds5$Country)

dataset$Country <- factor(dataset$Country,
                          levels = c('Germany','France','Spain'),
                          labels = c(1,2,3))
dataset
levels(dataset$Country)

ds6 <- ds4 %>% 
  mutate(
    Country = factor(Country,
                     levels = c('Germany','France','Spain'),
                     labels = c(1,2,3)),
    Purchased = factor(Purchased,
                       levels = c('No','Yes'),
                       labels = c(0,1))
  )
levels(ds6$Country)
str(ds6)

# f <- factor(c("a", "b", "c", "d"), levels = c("b", "c", "d", "a"))
dataset

# Splitting the Dataset into Training set and Test set ---------------------
install.packages("caTools")
library(caTools)

# sample.split, subset where split ==true, subset where split ==false

#Purchased is the dependent value
set.seed(123)
split <- sample.split(ds6$Purchased, SplitRatio = 0.8)
split
# true is the training set and false to the test set

training_set <- subset(ds6, split == TRUE) # training set
training_set

test_set <- subset(ds6,split == FALSE)
test_set

set.seed(1234)
split1 <- sample.split(ds6$Purchased, SplitRatio = 0.8)
split1

training_set1 <- subset(ds6, split == TRUE)
training_set1

test_set1 <- subset(ds6, split == FALSE)
test_set1

# Feature Scaling ---------------------------------------------------------











































































