library(boot)
library(tree)

wheatKernelDS <- read.csv("wheat_kernels.csv")
wheat_modified <- wheatKernelDS ## created a copy of the data set
attach(wheatKernelDS)
set.seed(39)

######################################################################
####################### Data Exploration ##############################

head(wheatKernelDS)
dim(wheatKernelDS)
summary(wheatKernelDS)
wheatKernelDS$Type <- as.factor(Type)
str(wheatKernelDS)

## checking if there is any null value in the data set
sum(is.na(wheatKernelDS))

## see the pairs
pairs(wheatKernelDS[,1:(length(wheatKernelDS) - 1)], 
      panel = panel.smooth, col = c(3,4,7))
## Correlation matrix
cor(wheatKernelDS[,1:(length(wheatKernelDS) - 1)])
## Covairance matrix
cov(wheatKernelDS[,1:(length(wheatKernelDS) - 1)])

## Creating a box plot visualisation
labels <- c("Kama", "Rosa", "Canadian")
par(mfrow = c(4,2))
boxplot(Area~Type,names = labels, col = c(3,4,7))
boxplot(Perimeter~Type,names = labels, col = c(3,4,7))
boxplot(Compactness~Type,names = labels, col = c(3,4,7))
boxplot(Length_of_Kernel~Type,names = labels, col = c(3,4,7))
boxplot(Width_of_Kernel~Type,names = labels, col = c(3,4,7))
boxplot(Assymetry_coefficient~Type,names = labels, col = c(3,4,7))
boxplot(Length_of_Kernel_Groove~Type,names = labels, col = c(3,4,7))


######################################################################
####################### Data Pre-processing ##########################

# Forming a new variable is_Kama that will contain to values mainly 0 and 1.
is_kama <- wheat_modified$Type
is_kama[is_kama != 1] = 0

## adding a new column is_kama to the data frame.
wheat_modified <- cbind(wheat_modified, is_kama)

# subsetting the data set to remove Type Column because we don't need it
wheat_modified <- subset(wheat_modified, select = -Type)
attach(wheat_modified)
head(wheat_modified)
## converting the is_kama variable as a factor variable
wheat_modified$is_kama <- as.factor(wheat_modified$is_kama)
str(wheat_modified)



## Dividing the data into training and testing data set.

# Dividing into 70% training data and 30% testing data.
training_indexes <- sample(1:nrow(wheat_modified), 
                           nrow(wheat_modified) * 0.7)

wheat_training <- wheat_modified[training_indexes,]
wheat_testing <- wheat_modified[-training_indexes,]

######################################################################
####################### Logistic Regression ##########################

lr_model1 <- glm(is_kama~., data = wheat_training, family = binomial)
summary(lr_model1)

## From this model we get to know that except Width_of_Kernel, each variable
## is significant.

######################################################################
## making a second model by using significant variables of first lr model..

lr_model2 <- glm(is_kama~Area+Perimeter+Compactness+Length_of_Kernel+
                   Assymetry_coefficient+Length_of_Kernel_Groove, 
                 data = wheat_training, family = binomial)
summary(lr_model2)

## Significant variables are : Area, Compactness, Assymetry_coefficient

######################################################################
## making a third model by using significant variables of second lr model..

lr_model3 <- glm(is_kama~Area+Compactness+
                   Assymetry_coefficient+Length_of_Kernel_Groove, 
                 data = wheat_training, family = binomial)
summary(lr_model3)

######################################################################

lr_model4 <- glm(is_kama~Area+
                   Assymetry_coefficient+Length_of_Kernel_Groove, 
                 data = wheat_training, family = binomial)
summary(lr_model4)

##### Cross Validation
dev.off()
model <- c(1:4)
cv_errorKF= rep (0,4)
cv_errorKF[1] <- cv.glm(wheat_training,lr_model1, K=10)$delta[1] 
cv_errorKF[2] <- cv.glm(wheat_training,lr_model2, K=10)$delta[1] 
cv_errorKF[3] <- cv.glm(wheat_training,lr_model3, K=10)$delta[1]
cv_errorKF[4] <- cv.glm(wheat_training,lr_model4, K=10)$delta[1]

cv_errorKF
plot(cv_errorKF~model, type="b", col="green",
     ylab = "Error rate", main = "10 Fold Cross Validation for models",
     xaxt="n", xlim = c(1,4)) 
axis(1, at = 1:4)
#lines(cv_errorKF+0.01, type="b", col="red")
######################################################################
#### Validation

calculate_misclass_rate <- function(model){
  lr_prediction1 <- predict(lr_model2, newdata = wheat_testing, 
                            type = "response")
  ## creating a prediction class for misclassification rate
  lr_predicted_class_1 <- rep(0, nrow(wheat_testing))
  
  lr_predicted_class_1[lr_prediction1 > 0.5] = 1
  
  ## Calculating misclassification rate
  
  lr_misclassification_table1 <- table(lr_predicted_class_1,
                                       wheat_testing$is_kama)
  
  
  cat("Misclassification matrix: ")
  print(lr_misclassification_table1)
  
  lr_misclassification_rate1 <- ((lr_misclassification_table1[1,2] + 
                                    lr_misclassification_table1[2,1]) 
                                 / sum(lr_misclassification_table1))
  lr_misclassification_rate1
  return(lr_misclassification_rate1)
}

lr_misclassification_rate1 <- calculate_misclass_rate(lr_model1)
lr_misclassification_rate2 <- calculate_misclass_rate(lr_model2)
lr_misclassification_rate3 <- calculate_misclass_rate(lr_model3)
lr_misclassification_rate4 <- calculate_misclass_rate(lr_model4)


cat("Model1 Missclassification rate:", lr_misclassification_rate1*100,
    "%\nModel2 Missclassification rate:", lr_misclassification_rate2*100,
    "%\nModel3 Missclassification rate:", lr_misclassification_rate3*100,
    "%\nModel4 Missclassification rate:", lr_misclassification_rate4*100,
    "%\n")

########### Evaluation
## From misclassification and cross validation, model 2 is better model.

##############################################################################
##############################################################################


######################################################################
####################### Decision Tree ################################

full_tree <- tree(is_kama~., data = wheat_modified)
summary(full_tree)

plot(full_tree)
text(full_tree,pretty = 0)

###### Tree on training data set

class_tree_training <- tree(is_kama~., data = wheat_training)
summary(class_tree_training)
plot(class_tree_training, 
     main = "Classification tree on training data")
text(class_tree_training, pretty = 0)


####### Testing the model accuracy

pred_tree1 <- predict(class_tree_training, newdata = wheat_testing, type = "class")

observed_test <- wheat_testing[, "is_kama"]

misclassification_matrix <- table(pred_tree1, observed_test)
misclassification_matrix

misclassrate <- ( misclassification_matrix[1,2] + misclassification_matrix[2,1] )/ sum(misclassification_matrix)

misclassrate ## 14.28% error rate


#######################################################################
######### Cross Validation

cv_classtree <- cv.tree(class_tree_training, FUN = prune.misclass)
cv_classtree
plot(cv_classtree$size, cv_classtree$dev, type = "b", xlab = "Tree Size", 
     ylab = "Number of Misclassifications", 
     main = "Cross Validation", col = "blue")

#######################################################################
######### Evaluation

### From this seems like best tree is of size 5, which have deviance = 19


#######################################################################
######### Testing

pruned_classtree <- prune.misclass(class_tree_training, best = 5)
summary(pruned_classtree)
plot(pruned_classtree, main = "Pruned tree with 5 terminal nodes")
text(pruned_classtree, pretty = 0)

#### Testing the accuracy / Calculating misclassification rate

pred_tree2 <- predict(pruned_classtree, newdata = wheat_testing, type = "class")

observed_test <- wheat_testing[, "is_kama"]

misclassification_matrix_pruned <- table(pred_tree2, observed_test)
misclassification_matrix_pruned

misclassrate_pruned <- ( misclassification_matrix_pruned[1,2] + misclassification_matrix_pruned[2,1] )/ sum(misclassification_matrix_pruned)

misclassrate_pruned

## 14.28% error rate on the best pruned tree

### Describe the terminal nodes of the resulting decision tree.

pruned_classtree


##############################################################################
##############################################################################

########################################################################
####################### Model Comparison ###############################

cat("Best Logic Regression Linear Model's Missclassification rate:"
    , lr_misclassification_rate2*100,"%\nBest Decision Tree's Missclassification rate:", 
    misclassrate_pruned*100,"%\n")

##############################################################################
##############################################################################

########################################################################
####################### Clustering #####################################
set.seed(39)
wheat_clustering <- wheat_modified[,1:7]

km_cluster2 <- kmeans(wheat_clustering, centers = 2, nstart = 20)
km_cluster2


km_cluster3 <- kmeans(wheat_clustering, centers = 3, nstart = 20)
km_cluster3


km_cluster4 <- kmeans(wheat_clustering, centers = 4, nstart = 20)
km_cluster4

round(km_cluster2$centers, 2)
round(km_cluster3$centers, 2)
round(km_cluster4$centers, 2)

#######################
clusters1 <- c(1:10)
withiness1 <- c()

for(i in clusters1){
  tot_with1 <- kmeans(wheat_clustering, centers = i, nstart = 20)$tot.withinss
  withiness1 <- c(withiness1, tot_with1)
}

plot(withiness1~clusters1, type="b", col="green",
     xlab = "Number of clusters", ylab = "Total withinss",
     main = "Total withinss vs Clusters")
########################################################################
########  Visualisation using PCA

pca_wheat <- prcomp(wheat_modified[,1:7], scale. = TRUE)
plot(pca_wheat$x[,1:2], col = km_cluster3$cluster + 1,
     main = "Visualisation of cluster with 3 groups")
legend("bottomleft", 
       legend=c("cluster 1","cluster 2", "cluster 3"), title="Clusters",
       col = c(2, 3, 4), pch = 1)

v <- rep(2, 77)
v <- c(v, rep(3, 72))
v <- c(v, rep(4, 61))
ggplot(data = val, mapping = aes(x = PC1, y = PC2)) +
  geom_point(col = v, shape = 1, size = 2.5)
