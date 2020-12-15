# Practice-Reinforcement Learning-Upper Confidence Bound ------------------
# Importing the data set --------------------------------------------------
# Multi armed bandit problem
# different versions of the add has been put on the website and the level of
# click is observed, CTR click Through Rates

library(tidyverse)

dt <- read.csv("Ads_CTR_Optimisation.csv")
glimpse(dt)
view(dt)

# Implementing the UCB from Scratch ---------------------------------------

# Step 1 - at each round we consider 2 numbers for each ad i; Ni;Ri
#Ni- number of times the ad i was selected up to round n
#Ri - the sum of rewards of the ad i up to round n
# Step 2 compute the average reward of ad i up to round n
# compute the confidence interval
# Step 3 we select the ad i that has the maximum UCB

N <-  10000
d <-  10
# initialise the vectors necessary in the algorithm
ads_selected <- integer(0) # empty vector
numbers_of_selections <- integer(d) # vector of zeros
sum_of_rewards <- integer(d) # a vector of zeros
total_reward <- 0

# the average reward of ad i up to round n (0 - 10000)

for (n in 1:N) {
  ad = 0 #initialise the index track
  max_upper_bound = 0 # initialise the upper bound for each ad
  # Calculate the second step and 3rd step
  for (i in 1:d){
    if (numbers_of_selections[i] > 0) {
      average_reward = sum_of_rewards[i] / numbers_of_selections[i]
      delta_i = sqrt(3/2*log(n)/numbers_of_selections[i])
      upper_bound = average_reward + delta_i
    } else {
      upper_bound =1e400 
    }
   # compare and update the max upper bound for each initialisation 
    if (upper_bound > max_upper_bound){
      max_upper_bound = upper_bound
  # keep track of the index of upper bound
      ad = i
    }
  }
  ads_selected = append(ads_selected, ad)
  numbers_of_selections[ad] = numbers_of_selections[ad] + 1
  reward = dt[n, ad]
  sum_of_rewards[ad] = sum_of_rewards[ad] + reward
  total_reward = total_reward + reward
}

tail(ads_selected, 500)

# Visualising the results of the total reward -----------------------------

hist(ads_selected,
     col = 'red',
     main =  ' Histogram of ad Selection',
     xlab =  ' Ads',
     ylab = 'Number of times each Ad was selected')




































































