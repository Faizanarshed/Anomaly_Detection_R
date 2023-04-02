install.packages("tidyverse")
install.packages("e1071")
install.packages("caTools")
install.packages("class")
install.packages("FNN")
install.packages("Rlof")
# Complete Loading package
library(e1071)
library(caTools)
library(class)
library(FNN)
library(Rlof)
library(tidyverse)
climatedata <- read.csv("Desktop/Anomaly Detection/climatedata.csv")
View(climatedata)
head(climatedata)
climatedata <- climatedata[,-1]
#plot(humidity ~ meanpressure, data = climatedata)
plot(humidity ~ wind_speed, data = climatedata)
# Calculating the 5 nearest neighbors distance
climate_nn <- get.knn(climatedata$humidity, k = 5)

# Viewing the distance matrix
head(climate_nn$nn.dist)
climate_nn$nn.dist[5, 1]
# Row index of climate data 5's nearest neighbor 
climate_nn$nn.ind[5, 1]

# Returning data for climate data and its nearest neighbor
climatedata[c(5, 19), ]

#Calculating the 5 nearest neighbors distance
climate_nn <- get.knn(climatedata$humidity, k = 5)

# Creating score by averaging distances
climate_nnd <- rowMeans(climate_nn$nn.dist)

# Printing row index of the most anomalous point
which.max(climate_nnd)

summary(climatedata)

# Printing the 5-nearest neighbor distance score
climate_nnd[1:5]

# Indicating the score as a new column 
climatedata$score <- climate_nnd


# Scatterplot showing humidity and wind_speed kNN score
plot(humidity ~ wind_speed, data = climatedata, cex = sqrt(score), pch = 20,
     main = "Graph Showing KNN Score")


# Calculating the LOF(Local Outlier Factor) for climate data
climatedata_lof <- lof(scale(climatedata), k = 5)

# Adding the LOF score as a new column
climatedata$score <-  climatedata_lof
plot(humidity ~ wind_speed, data = climatedata, cex = score, pch = 20,
     main = "Graph Showing LOF(Local Outlier Factor) Record")

# Scaling climate data
climatedata_scaled <- scale(climatedata)

# Calculating and indicating kNN distance as a new column
climatedata_nn <- get.knn(climatedata_scaled, k = 10)
climatedata$score_knn <- rowMeans(climatedata_nn$nn.dist)     

# Calculating and indicating LOF as a new column
climatedata$score_lof <- lof(climatedata_scaled, k = 10)

# Finding the row location of highest kNN
which.max(climatedata$score_knn)
which.max(climatedata$score_lof)

boxplot(score_knn ~ score_lof, data = climatedata,main = "Graph Predicting KNN VS LOF")
barplot(score_knn ~ score_lof, data = climatedata,main = "Graph Predicting KNN VS LOF")

