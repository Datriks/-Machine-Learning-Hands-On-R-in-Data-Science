# Import the data set
library(tidyverse)
library(caTools)

Data <- read_csv("Part 1 - Data Preprocessing/Section 2 -------------------- Part 1 - Data Preprocessing --------------------/R/Data.csv")
View(Data)
Data

# in case we have to select a subset of data we use the following line
#Data <- Data[,2:3] # select the indexes we need to select

# Data Preprocessing- missing data ----------------------------------------
Data %>% 
  summarise_all(~ sum(is.na(.)))

Data1 <- Data %>% 
  mutate(
    Age = round(replace_na(Age,mean(Age, na.rm = T)),0),
    Salary = round(replace_na(Salary,mean(Salary,na.rm = T)),0)
  )
Data1

# Encoding Categorical data -----------------------------------------------

Data2 <- Data1 %>% 
  mutate(
    Country = factor(Country,
                     levels = c("Germany","France","Spain"),
                     labels = c(1,2,3)),
    Purchased = factor(Purchased,
                       levels = c("No","Yes"),
                       labels = c(0,1))
  )
Data2

# Splitting the dataset into training and test set ------------------------

Data3 <- Data2 %>% 
  mutate(
    id = 1:nrow(.)
  )
Data3

training_new <- Data3 %>% 
  sample_frac(0.8)
training_new

test_new <- Data3 %>% 
  anti_join(training_new, by = "id")
test_new

# Feature Scaling ---------------------------------------------------------

training_new[,2:3] <- scale(training_new[,2:3])
training_new <- training_new %>%
  select(1:4)
training_new 

test_new[,2:3] <- scale(test_new[,2:3])
test_new <- test_new %>% 
  select(1:4)
test_new







































































































