# Practice-Eclat-Association Rule-Machine Learning ------------------------
# optimise the sales to increase the revenue

library(tidyverse)
library(arules)

dt <- read_csv("Market_Basket_Optimisation.csv")
glimpse(dt)

dt <- read.transactions("Market_Basket_Optimisation.csv", 
                        sep = ',',
                        rm.duplicates = T)
summary(dt)

itemFrequencyPlot(dt, topN = 10)

# Training the Eclat on the dataset ---------------------------------------

rules <- eclat(data = dt, parameter = list(support =0.004, minlen = 2))


# Visualisation the result ------------------------------------------------

inspect(sort(rules, by = 'support')[1:10])















































































