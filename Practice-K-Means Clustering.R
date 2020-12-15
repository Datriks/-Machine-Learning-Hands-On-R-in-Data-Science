# K-Means Clustering ------------------------------------------------------

library(tidyverse)

dt <- read_csv("Mall_Customers.csv")
glimpse(dt)
view(dt)

# separate the customers into diff groups based on annual income and spending score

# Data Preprocessing ------------------------------------------------------
# select just the features that are deterministic in our analysis

x <- dt[4:5]
x

# Using the elbow method analysis -----------------------------------------

set.seed(6)
wcss <- vector()

# create a for loop
for (i in 1:10) {
  wcss[i] <- sum(kmeans(x,i)$withinss)
  }

plot(1:10, wcss, type = 'b', 
     main = paste('Clusters of Clients'),
     xlab = 'Number of Clusters',
     ylab = 'WCSS')

# based on the graph plotted we choose the number of clusters = 5 at the elbow

# Applying K-means to the mall dataset x ----------------------------------
set.seed(29)
kmeans <- kmeans(x,5, iter.max = 500, nstart = 10)

# Visualising the Clusters based on K-means Algorithm ---------------------
library(cluster)

clusplot(
  x,
  kmeans$cluster,
  lines = 0,
  shade = T,
  color = T,
  labels = 2,
  plotchar = F,#
  span = T,
  main = paste('Clusters of Clients'),
  xlab = 'Annual Income',
  ylab = 'Spending Score'
)

























































































