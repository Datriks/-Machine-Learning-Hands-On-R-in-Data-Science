# Hierarchical Clustering-Agglomerative -----------------------------------

library(tidyverse)
dt <- read_csv('Mall_Customers.csv')
glimpse(dt)
view(dt)

# Select the data of Interest ---------------------------------------------
# annual income and Spending Score
x <- dt[4:5]
x

# Using the Dendogram to find the optimal number of clusters --------------
# create e dendogram to find the optimal number of clusters k
dendogram <- hclust(dist(x, method = 'euclidean'),
                    method = 'ward.D')
plot(dendogram,
     main = paste('Dendogram'),
     xlab = 'Customers',
     ylab = 'Euclidean Distance')

# Fitting Hierarchical clustering to the data set -------------------------

hc <- hclust(dist(x, method = 'euclidean'),
             method = 'ward.D')
# 5 is the value detemined by the longest vertical dendogram line that is not 
# crossed by a horisontal line.

y_hc <- cutree(hc, 5)
y_hc

# Visualising the Clusters ------------------------------------------------
library(cluster)

clusplot(x,
         y_hc,
         lines = 0,
         shade = T,
         color = T,
         labels = 2,
         plotchar = F,
         span = T,
         main = paste('Clusters of Customers'),
         xlab = 'Anual Income',
         ylab = 'Spending Score')




































































































