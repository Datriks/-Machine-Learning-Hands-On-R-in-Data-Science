# Practice-Thomson Sampling-Reinforcement Learning ------------------------
# Algorithm Intuition
library(tidyverse)

dt <- read.csv("Ads_CTR_Optimisation.csv")
glimpse(dt)
#view(dt)

# multi arm bandit problem: we have d arms(ads images), 
# Step 1: at each round we consider 2 numbers for each i:
# the number of times ad i got reward 1 up to round n
# the number of times the ad i got reward 0 up to round n
# Step 2: for each ad i we take a random draw from the distribution
# Step 3: We select the ad that has the highest reward theta.

# Implementing the Thompson Algorithm -------------------------------------

N <-  10000
d <-  10
# initialise the vectors necessary in the algorithm
ads_selected <- integer(0) # empty vector
# Step 1 we consider 2 numbers for each draw
numbers_of_rewards_1 <- integer(d)
numbers_of_rewards_0 <- integer(d)
total_reward <- 0

# the average reward of ad i up to round n (0 - 10000)

for (n in 1:N) {
  ad = 0 #initialise the index track- the ad
  max_random = 0 # initialise the max random draw for each ad
  # Calculate the second step and 3rd step
  for (i in 1:d){
# Step 2 we consider 2 numbers
# random generation for the Beta distribution 
   random_beta = rbeta(n = 1,
                       shape1 = numbers_of_rewards_1[i] + 1,
                       shape2 = numbers_of_rewards_0[i] + 1)
# compare and update the max upper bound for each initialisation 
    if (random_beta > max_random){
      max_random = random_beta
      # keep track of the index of upper bound
      ad = i
    }
  }
  
  ads_selected = append(ads_selected, ad)
  reward = dt[n, ad]
# increment the number of rewards
  if ( reward == 1) {
    numbers_of_rewards_1[ad] = numbers_of_rewards_1[ad] + 1
  } else {
    numbers_of_rewards_0[ad] = numbers_of_rewards_0[ad] + 1
  }
# Step 3
  total_reward = total_reward + reward
}

tail(ads_selected, 500)

# Visualising the results of the total reward -----------------------------

hist(ads_selected,
     col = '#890000',
     main =  ' Histogram of ad Selection',
     xlab =  ' Ads',
     ylab = 'Number of times each Ad was selected')





































































