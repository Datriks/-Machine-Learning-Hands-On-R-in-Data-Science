# Market Basket Optimisation --Apriori---Association Rule-----------------------------------------
# Optimasing Sales
install.packages('arules') # this is for creating sparse matrix (0-1)

library(tidyverse)
library(arules)

dt <- read.csv('Market_Basket_Optimisation.csv', header = F)
dt
glimpse(dt)

# Transform our data into a sparse matrix ---------------------------------
# we gonna end up with each column for each product = 120
# the lines would stay the same as all products that each customer bought but would 
# be noted with 1 and 0.

dt1 <- read.transactions('Market_Basket_Optimisation.csv', 
                        sep = ',', 
                        rm.duplicates = T)
summary(dt1)


# Visualise the distribution of the baskets -------------------------------
# Provides the generic function itemFrequencyPlot and the S4 method to 
# create an item frequency bar plot for inspecting the item frequency 
# distribution for objects based on itemMatrix (e.g., transactions, or items in item sets and rules).

itemFrequencyPlot(dt1, topN = 10) # topN first 10 frequencies or 100

# Training the Apriori model on the data set ------------------------------
# we have to choose a value for the support and the graph is going to help
# support is given by the number of items/nr of transactions
# we consider the items bought 3 times a day for 7 days in a week we have 21 items
# then we divide 21/7500 transactions to obtain the minimum support.
# confidence is chosen in an arbitrary way but not too high or too small,
# default is 0.8; but is to high after test
# choose 0.4 for confidence to obtain a number of rules

rules <- apriori(dt1, parameter = list(support = 0.004, confidence = 0.2))

# Visualise the results - look at the rules explicitly -------------------
# sort the rules by decreasing lift use the inspect function

inspect(sort(rules,by = 'lift')[1:10])

# change the confidence , reduce it, confidence divide by 2 again and have 0.2
# we looking at first 10 rules with the highest lifts
# people that buy mineral water and whole pasta also buy olive oil in 40% of the cases(confidence)




































































































