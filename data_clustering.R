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
