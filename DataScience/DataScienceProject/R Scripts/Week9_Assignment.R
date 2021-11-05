library(ISLR)
irisDS <- iris
View(irisDS)
head(irisDS)
attach(irisDS)
dim(irisDS)
str(irisDS)

## in clustering we try to divide the data into groups as per their similarities.

## Part (a): k-means clustering
## 1. Cluster the dataset into 2 groups using k-means clustering. 
# in clustering we ignore the categorical variable and only use predictor variables

km.iris <- kmeans(irisDS[, 1:4], centers = 2, nstart = 20)
# centers = 2 means 2 clusters.
# nstart = 20 means it will assign each observation to each cluster only 1 time.

# like it will run this 20 time
# kmeans(irisDS[,1:4], centers = 2, nstart = 1)
# and then return the optimal value for total.withiness sum of squres.


### How to find the optimal number of clusters

## we can plot the data with number of clusters and their total.withiness
## and see when the sum of squres ( total)  is lower and then that will be the optimal
## number of clusters
km.iris

# cluster centroids are cluster means, in this we have two clusters
km.iris$cluster
## this shows which observations belong to which clusters
km.iris$centers
# this will give cluster centroids
km.iris$tot.withinss
# this is total sum of squares of clusters
km.iris$withinss
# this will give sum of squares of individual clusters



## 2. Draw a plot to visualize the clusters.
## for this visualisation we can do by PCA
pca.iris <- prcomp(irisDS[,1:4], scale. = TRUE)
plot(pca.iris$x[,1:2], col = km.iris$cluster + 1)


## 3. Cluster the dataset into 3 groups using k-means clustering. 
km.iris3 <- kmeans(irisDS[, 1:4], centers = 3, nstart = 20)
# centers = 2 means 2 clusters.
km.iris3
km.iris3$tot.withinss
# this means the observations are much more closer than the 2 clusters we identifies at
# question 1.

# with this clusters with 3 are much better than clusters with 2.

## 4. Draw a plot to visualize the clusters.
plot(pca.iris$x[,1:2], col = km.iris3$cluster + 1)

## let's see what happen when we divide the cluster into 4 clusters
km.iris4 <- kmeans(irisDS[, 1:4], centers = 4, nstart = 20)
# centers = 2 means 2 clusters.
km.iris4
km.iris4$tot.withinss ## value is reduced from 3

plot(pca.iris$x[,1:2], col = km.iris4$cluster + 1)

## Part (b): Hierarchical Clustering

## in this, we don't need to decide the number of clusters before hand,
## here, we do clustering and then decide the number of clusters
## first each of the observations divide into each clusters
## then we calculate the distance between each obs and the one which are close to them are combines
## likwise, we keep on combining the obs until each obs falls into a cluster



## 1. Cluster the observations using complete linkage.
hh.complete <- hclust(dist(iris[,1:4]), method = "complete")
hh.complete


## 2. Cluster the observations using average and single linkage. 
hh.average <- hclust(dist(iris[,1:4]), method = "average")
hh.average

hh.single <- hclust(dist(iris[,1:4]), method = "single")
hh.single

## 3. Plot the dendrogram for the above clustering methods.

plot(hh.complete, main = "Complete Linkage")
plot(hh.average, main = "Average Linkage")
plot(hh.single, main = "Single Linkage")

## from this we can decide how many clusters we want
## in the leaves it will show observation numbers ( if we don;t have row names)

## if we cut this plot based on heights, that will give us the clusters
rect.hclust(hh.complete, k = 2)
rect.hclust(hh.complete, k = 3)
rect.hclust(hh.complete, k = 4)

# we now want to know which observations goes into which clusters
cutree(hh.complete, k = 4)

## now we want to see how we see this using plot
plot(pca.iris$x[,1:2], col = cutree(hh.complete, k = 2) + 1)


## compare the 2 methods of clustering

par(mfrow = c(1,2))
plot(pca.iris$x[,1:2], col = km.iris$cluster + 1, main = "K-means")
plot(pca.iris$x[,1:2], col = cutree(hh.complete, k = 2) + 1, main = "heirarchical ")

## now numbers of clusters -> 

## Question 2
## enerate the data set using the following codes
set.seed(2)
x = matrix(rnorm(50*2), ncol = 2) 
x[1:25, 1]= x[1:25, 1] + 3 
x[1:25, 2] = x[1:25, 2] - 4
class(x)
x <- as.data.frame(x)
head(x)
dim(x)

km.mat <- kmeans(x, centers = 2, nstart = 20)
km.mat
km.mat$tot.withinss

pca.mat <- prcomp(x, scale. = TRUE)
plot(pca.mat$x[,1:2], col = km.mat$cluster + 1)

km.mat1 <- kmeans(x, centers = 3, nstart = 20)
km.mat1
km.mat1$tot.withinss

pca.mat1 <- prcomp(x, scale. = TRUE)
plot(pca.mat1$x[,1:2], col = km.mat1$cluster + 1)

## Compare the total within-cluster sum of squares when nstart = 1 and 20.
km.mat_1 <- kmeans(x, centers = 2, nstart = 1)
km.mat_1$tot.withinss

km.mat1_1 <- kmeans(x, centers = 3, nstart = 1)
km.mat1_1$tot.withinss

cat("Total within cluster sum of squares when nstart = 1 is ", km.mat_1$tot.withinss)
cat("Total within cluster sum of squares when nstart = 20 is ", km.mat$tot.withinss)

cat("Total within cluster sum of squares when nstart = 1 is", km.mat1_1$tot.withinss)
cat("Total within cluster sum of squares when nstart = 20 is", km.mat1$tot.withinss)


## Part (b): Hierarchical Clustering
## 1. Cluster the observations using complete linkage.
hh.complete_mat <- hclust(dist(x), method = "complete")
hh.complete_mat


## 2. Cluster the observations using average and single linkage. 
hh.average_mat <- hclust(dist(x), method = "average")
hh.average_mat

hh.single_mat <- hclust(dist(x), method = "single")
hh.single_mat

## 3. Plot the dendrogram for the above clustering methods.

plot(hh.complete, main = "Complete Linkage")
plot(hh.average, main = "Average Linkage")
plot(hh.single, main = "Single Linkage")
## 2. Cluster the observations using average and single linkage. 
## 3. Plot the dendrogram for the above clustering methods.