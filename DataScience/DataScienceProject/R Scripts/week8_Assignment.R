#################################
## Principal Component Analysis
##
#################################

## we want our Principal component to be perpendicular


# 1. Load “iris” dataset from the ISLR Library.
library(ISLR)
attach(iris)
# 2. Give a short description of the dataset.
head(iris)
str(iris)
summary(iris)
dim(iris)
cor(iris[,1:4])
# 3. Examine the mean of the variables in the dataset.
iris_DS <- iris[,1:4]
sapply(iris_DS, mean)
# 4. Examine the variance of the variables in the dataset.
sapply(iris_DS, var)
# 5. Perform Principal Component Analysis for the iris dataset.
pr.out.iris <- prcomp(iris_DS, scale. = TRUE)
pr.out.iris$scale
# 6. Display and explain the principal component loading vectors.
pr.out.iris$rotation
# 7. Give the first two Principal components using loading parameters in part 6.
pc1.iris.vector <- pr.out.iris$rotation[,1]
PC1.iris <- pc1.iris.vector[[1]] * Sepal.Length + pc1.iris.vector[[2]]* Sepal.Width +
            pc1.iris.vector[[3]] * Petal.Length + pc1.iris.vector[[4]]* Petal.Width

pc2.iris.vector <- pr.out.iris$rotation[,2]
PC2.iris <- pc2.iris.vector[[1]] * Sepal.Length + pc2.iris.vector[[2]]* Sepal.Width +
            pc2.iris.vector[[3]] * Petal.Length + pc2.iris.vector[[4]]* Petal.Width
PC1.iris; PC2.iris;
# 8. Explain the proportion of variance explained by each PC using graphs and summarise your results.
summary(pr.out.iris)
pr.out.iris$sdev
pr.out.iris$var <- pr.out.iris$sdev ^ 2
PVE.iris <- pr.out.iris$var / sum(pr.out.iris$var)
PVE.iris
plot(PVE.iris, type = "b", xlab = "Pricipal Components", ylab = "Proportions of Variance explained")
# cumulative PVE
plot(cumsum(PVE.iris), type="b", xlab = "Pricipal Components", 
     ylab = "Cumulative Proportions of Variance explained")
# 9. Draw the biplot and interpret it.
biplot(pr.out.iris, scale = 0)
# 10. Rotate the graph and compare the results.
pr.out.iris$rotation <- -pr.out.iris$rotation
pr.out.iris$x <- -pr.out.iris$x
biplot(pr.out.iris, scale = 0)
### Question 2

# 1. Load the “USArrests” dataset built in R.
arrest <- USArrests
attach(arrest)
# 2. Give a short description of the dataset.
# here we need to do, dim, str, head etc.
head(arrest)
dim(arrest)
str(arrest)
summary(arrest)
cor(arrest)

# 3. Examine the mean of the variables in the dataset.
apply(arrest, MARGIN = 2, mean)
## or
sapply(arrest, mean)
## or
colMeans(arrest)

# 4. Examine the variance of the variables in the dataset.
apply(arrest, MARGIN = 2, var)
## or
sapply(arrest, var)
## each variable the variance is very different.

# 5. Perform Principal Component Analysis for USArrests.
pr.out.arrest <- prcomp(arrest, scale. = TRUE)
pr.out.arrest
# each of the value data in PC1,2..4 is Pricipal component loading and PC1
# is called principal component loading vectors
pr.out.arrest$center
# center will give the mean of the original variables.
pr.out.arrest$scale
# sclae will give the SD of the original variables.
pr.out.arrest$sdev
# sdev will give the SD of principal component scores.
pr.out.arrest$x
# x will give the PC scores. It is calculated by using the equation 


# 6. Display and explain the principal component loading vectors.
pr.out.arrest$rotation
# for weightage we do not consider sign as of now.
# this means is what is the weight given to each variable.
# here, in PC1 we can see that Murder, Assault, Rape gives equal and majority contributions
# in PC2 UrbanPop gives majority contributions.
# PC1 => Murder, Assault, Rape -  we can see this as a representation of crime variable,
# PC2 => Urban Pop - it explains the level of urbanised populations
# PC3 => 

# 7. Give the first two Principal components using loading parameters in part 6.
pc1.arrest.vector <- pr.out.arrest$rotation[,1]
PC1.arrest <- pc1.arrest.vector[[1]]*Murder + pc1.arrest.vector[[2]]*Assault + 
       pc1.arrest.vector[[3]]*UrbanPop + pc1.arrest.vector[[4]]*Rape
pc2.arrest.vector <- pr.out.arrest$rotation[,2]
PC2.arrest <- pc2.arrest.vector[[1]]*Murder + pc2.arrest.vector[[2]]*Assault + 
       pc2.arrest.vector[[3]]*UrbanPop + pc2.arrest.vector[[4]]*Rape
PC1.arrest; PC2.arrest
# 8. Explain the proportion of variance explained by each PC using graphs and summarise your results. 
summary(pr.out.arrest)
# most proportion of variance explained by PC1
# it is used as preprocessing technique. 

# Proportion of variance explained
pr.out.arrest$sdev
pr.out.arrest$var <- pr.out.arrest$sdev ^ 2
PVE.arrest <- pr.out.arrest$var / sum(pr.out.arrest$var)
PVE.arrest
plot(PVE.arrest, type = "b", xlab = "Pricipal Components", ylab = "Proportions of Variance explained")

# this is a graph,an elbow method.

plot(cumsum(PVE), type="b", xlab = "Pricipal Components", 
     ylab = "Cumulative Proportions of Variance explained")

# 9. Draw the biplot and interpret it.
biplot(pr.out.arrest, scale = 0)
# scale  = 0 means to make sure the loadings are exactly the same.
## interpretations
# there are 3 different things to consider
# how much weightage or what are the contributions given by the original variable to each?
# Answer: More variables are close to horizontal axis(PC1) are Murder, Assault and Rape. Meaning they have 
# more weigthage toward PC1
# on the other hand UrbanPop have more weightage towards PC2



# which of the variables are associated with each other or correlated with each other.
# Answer: if the angle between the two variables are small then they are highly positively correlated.
# if the angle is 90, this means there is no correlation. we can compare with Murder~UrbanPop no much
# correlation.

# by looking at the observations and variables together.
# here we can see Mississippi, North Carolina is in the same direction of Murder, that means
# they have high Murder rate but South Dakota, is having less murder rate.

# that is how we can compare them. However, the variables that are at 0,0 have average rate for Murders
# assualt and rape rate.



# 10. Rotate the graph and compare the results.
pr.out.arrest$rotation <- -pr.out.arrest$rotation
pr.out.arrest$x <- -pr.out.arrest$x
biplot(pr.out.arrest, scale = 0)

