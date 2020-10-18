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
