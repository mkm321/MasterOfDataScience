## Question 1 Regression Trees: Use “Carseats” dataset with Sales as the Target Variable
## Disdavantage of decision trees is that it is not robust.
# Attach the “Carseats” dataset from ISLR library and explore.
library(ISLR)
library(tree)
attach(Carseats)
head(Carseats)
dim(Carseats)
str(Carseats)

## a) Divide the dataset into Training and Testing sets.
set.seed(5)
trainingIndexes <- sample(1:nrow(Carseats), nrow(Carseats)* 0.5)

training_DS <- Carseats[trainingIndexes, ]
dim(training_DS)

testing_DS <- Carseats[-trainingIndexes,]
dim(testing_DS)

# b) Fit a Regression Tree for training data set.

reg_tree <- tree(Sales~., data = Carseats, subset = trainingIndexes)
summary(reg_tree)

plot(reg_tree)
text(reg_tree, pretty = 0)



# For regression we uses MSE to test the model accuracy.

# Full model accuracy is : 

yhat1 <- predict(reg_tree, newdata = testing_DS)

MSE <- mean((yhat1 - testing_DS[, "Sales"])^2)
MSE
## now the root mean squared error is  : 
RMSE1 <- sqrt(MSE)
RMSE1

# c) Construct the Cross validation plot 

set.seed(6)
cv_reg_tree <- cv.tree(reg_tree)
cv_reg_tree

## here deviance is sum of squared erros

plot(cv_reg_tree$size, cv_reg_tree$dev, type = "b", xlab = "Tree Size", 
     ylab = "Sum of Squared Errors (SSE)")

# d) Select the best size of the tree

# Best size looks is a fully grown tree which is tree with size 20.

# in this case we are selecting the fully grown tree


# e) Obtain the best regression tree by pruning

pruned_regtree <- prune.tree(reg_tree, best = 20)
plot(pruned_regtree)
text(pruned_regtree, pretty = 0)



# f) Test the model accuracy.

yhat_pruned <- predict(pruned_regtree, newdata = testing_DS)

MSE_pruned <- mean((yhat_pruned - testing_DS[, "Sales"])^2)
MSE_pruned
## now the root mean squared error is  : 
RMSE_pruned <- sqrt(MSE_pruned)
RMSE_pruned

# g) Describe the terminal nodes of the resulting decision tree

pruned_regtree



## Question 2

# a) Transfer Sales Variable from a Continuous variable to a Categorical Variable. 
# Assign Assign Sales as “Yes” if Sales value is greater than 8 and “No” otherwise.
hist(Sales)
HighSales <- ifelse(Sales <= 8, "NO", "YES")
head(HighSales)
table(HighSales)

CarSeatsNew <- data.frame(Carseats, HighSales)
CarSeatsNew <- CarSeatsNew[, -1]
head(CarSeatsNew)
str(CarSeatsNew)
CarSeatsNew$HighSales = as.factor(CarSeatsNew$HighSales)
str(CarSeatsNew)

# CarSeatsNew <- data.frame(CarSeatsNew[, -1], HighSales)
# b) Fit a Classification Tree for the full data set.
class_tree_full <- tree(HighSales~., data = CarSeatsNew)
# . commadning R to use all the variables
summary(class_tree_full)
plot(class_tree_full)
text(class_tree_full,pretty = 0)
# summary will give misclassiication rate that is  9%
# c) misclassiication rate that is  9%
# to be more confident we divide the data into training and testing data set.
# if misclassigivation rate is as low as this in testing data set then we can say
# this is goof model

# d) Divide the dataset into Training and Testing sets and fit a Classification Tree for training dataset.

set.seed(5)
tr <- sample(1:nrow(CarSeatsNew), nrow(CarSeatsNew) * 0.5)

CarSeats_Training <- CarSeatsNew[tr, ]
dim(CarSeats_Training)
CarSeats_Testing <- CarSeatsNew[-tr, ]
dim(CarSeats_Testing)

# Classigication tree for training DS
class_tree_training <- tree(HighSales~., data = CarSeats_Training)
summary(class_tree_training)
plot(class_tree_training)
text(class_tree_training, pretty = 0)

## So here we can see that misclassification rate is 10.5%
## here we used less variables to build the tree.
## misclassification rate is calculated by 
# e) Test the model accuracy.

## We can test the model accuracy by, using the test data set.

pred_class_test1 <- predict(class_tree_training, newdata = CarSeats_Testing, type = "class")
head(pred_class_test1)

observed_test <- CarSeats_Testing[, "HighSales"]

misclassification_matrix <- table(pred_class_test1, observed_test)
misclassification_matrix

misclassrate <- ( misclassification_matrix[1,2] + misclassification_matrix[2,1] )/ sum(misclassification_matrix)

misclassrate

# we can compare this value with test data set and see misclassigicaiton rate is higher in testing than testing.

# f) Construct the Cross validation plot.

set.seed(1)
cv_classtree <- cv.tree(class_tree_training, FUN = prune.misclass) ## we give this function for classification problem,
# else we set K value
cv_classtree
plot(cv_classtree$size, cv_classtree$dev, type = "b", xlab = "Tree Size", 
     ylab = "Number of Misclassifications")
# the dev give the value of how many number of miscalssification is here.
# K is called cost complexity pruning, it is a parameter to prune the tree



# g) Select the best size of the tree model.
# From 66 we can see 53 is less. so when tree size is 6 prun tree produces better results in cross validation
# so we will go with pruned tree => best = 6

## it is like deciding the degrees of the pilynomical



## 

# h) Obtain the best classification tree by pruning.

## for this we will use 10 fold cross validation, and keep on chopping the branches and then by considering the error rate for
## cross validation, we can decide which tree to choose



pruned_classtree <- prune.misclass(class_tree_training, best = 6)
plot(pruned_classtree)
text(pruned_classtree, pretty = 0)


# i) Test the model accuracy.

pred_class_test2 <- predict(pruned_classtree, newdata = CarSeats_Testing, type = "class")
head(pred_class_test2)

observed_test <- CarSeats_Testing[, "HighSales"]

misclassification_matrix_pruned <- table(pred_class_test2, observed_test)
misclassification_matrix_pruned

misclassrate <- ( misclassification_matrix_pruned[1,2] + misclassification_matrix_pruned[2,1] )/ sum(misclassification_matrix_pruned)

misclassrate
# j) Describe the terminal nodes of the resulting decision tree.

# explain the tree
pruned_classtree
## Asteric mark separates the terminal node with other node
## Terminal nodes are ending nodes, in this case YES, NOs are the terminal modes
