# Advanced stats week 3
# Exercise 5: Data clustering with standard continuous data

soils <- read.csv("~/profskills/Peru_Soil_Data.csv", row.names=1, stringsAsFactors=T)
names(soils)

# select only coninuous variables and make distance matrix
# using default: euclidian distance
soils_distances <- dist(soils[,c(4:19)])

# execute hierarchial clustering
soils_cluster <- hclust(soils_distances)
plot(soils_cluster)

# look at distribution of variables
# some variables have a low range of variation (soil pH from 4 to 6.7)
# some have a large range of variation (Calcium from 39 to 2292
# they need to be standardised
summary(soils[,c(4:19)])

# scale variables
soils_scaled <- scale(soils[,c(4:19)])

# re-run distances
soils_distances <- dist(soils_scaled)

# re-cluster using default method = "complete"
soils_cluster <- hclust(soils_distances)
plot(soils_cluster)


# Exercise 6: Clustering with species composition data ####
# dealing with skewed / zero-inflated / count data

library(tidyverse)

inga <- read.csv("~/profskills/Supplement_5_KGD.csv",row.names = 1)

# get column classes
str(inga)

# scale variables
inga_scaled <- scale(inga)

# get distances
inga_distances <- dist(inga_scaled)

# cluster
inga_cluster <- hclust(inga_distances,method="complete")

# plot dendrogram
plot(inga_cluster)

# Based on our knowledge of this system, we would think that upland and 
# floodplains house very different species
# but these results suggest otherwise
# this may be because we used an inappropriate measure of distance

# Base R does not manage these kinds of data well, but library(vegan) does

library(vegan)

inga_distances <- vegdist(inga)
inga_cluster <- hclust(inga_distances,method="ward.D2")
plot(inga_cluster)

# compare soils dendro to inga dendro in same plot window
par(mfrow=c(1,2))
plot(soils_cluster); plot(inga_cluster)
par(mfrow=c(1,1)) #to simply reset your plotting window

# It looks like species do a better job of differentiating habitats than our soils data

# if you look at a map of the sites in the original paper, you will see that the 
# species composition data show a stronger signal of geography than the soils

# Exercise 7: Principal Component Analyses