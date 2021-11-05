# predict whether a given car gets high or low gas mileage based on the “Auto” data set.



wheatKernelDS <- read.csv("wheat_kernels.csv")
attach(wheatKernelDS)
View(wheatKernelDS)
head(wheatKernelDS)
dim(wheatKernelDS)
summary(wheatKernelDS)
str(wheatKernelDS)


## find null/NA values."
sum(is.na(wheatKernelDS))
sum(is.na(wheatKernelDS$Area))
sum(is.na(wheatKernelDS$Perimeter))
sum(is.na(wheatKernelDS$Compactness))
sum(is.na(wheatKernelDS$Length_of_Kernel))
sum(is.na(wheatKernelDS$Width_of_Kernel))
sum(is.na(wheatKernelDS$Assymetry_coefficient))
sum(is.na(wheatKernelDS$Length_of_Kernel_Groove))
sum(is.na(wheatKernelDS$Type))

## R doesn't considered type as a factor variable, so converting it to factor variable with name
kama <- 1
rosa <- 2
canadian <- 3

## explore
par(mfrow = c(1,1))
plot(Area~Assymetry_coefficient,col=Type, data=wheatKernelDS, ylab= "Area")
legend("topleft", legend = c("Kama", "Rosa", "Canadian"), col = c(1,2,3), lty = 1)

## Pre-processing
wheatKernelDS$Type <- as.factor(Type)


## Area of the kernel

summary(Area)
hist(Area, breaks = 20)
wheatKernelDS[wheatKernelDS$Area > max(Area)]

# min is 10.59 and max is 21.18 and mean, median is 14.85 and 14.36 respectively. This shows
# area has some outliers towards end.


par(mfrow = c(2,2))
boxplot(Area)
boxplot(Area~Type)

## from this we get to know that Canadian wheat kernels are smaller in terms of area
## followed by Kama and Rosa has the highest area.

## there are some outliers observed in Kama as well.

hist(wheatKernelDS$Area[Type=="Kama"], breaks = 30)

## Perimeter

summary(Perimeter)
boxplot(Perimeter~Type)

labels <- c("Kama", "Rosa", "Canadian")
par(mfrow = c(4,2))
boxplot(Area~Type,names = labels, col = c(3,4,7))
boxplot(Perimeter~Type,names = labels, col = c(3,4,7))
boxplot(Compactness~Type,names = labels, col = c(3,4,7))
boxplot(Length_of_Kernel~Type,names = labels, col = c(3,4,7))
boxplot(Width_of_Kernel~Type,names = labels, col = c(3,4,7))
boxplot(Assymetry_coefficient~Type,names = labels, col = c(3,4,7))
boxplot(Length_of_Kernel_Groove~Type,names = labels, col = c(3,4,7))

## see the pairs
pairs(wheatKernelDS[,1:(length(wheatKernelDS) - 1)], panel = panel.smooth, col = c(3,4,7))
cor(wheatKernelDS[,1:(length(wheatKernelDS) - 1)])
cov(wheatKernelDS[,1:(length(wheatKernelDS) - 1)])
## divide into training and testing data set

tr <- sample(1:nrow(wheatKernelDS), nrow(wheatKernelDS) * 0.7)

training_DS <- wheatKernelDS[tr,]
testing_DS <- wheatKernelDS[-tr,]

## to find out if the wheat is canadian or not, we have to modified the data set and introduce new col
## Canadian
newDS <- wheatKernelDS
newDS$Type[newDS$Type != 3] = "No"
newDS$Type[newDS$Type == 3] = "Yes"

table(newDS$Type)
newDS$Type <- as.factor(newDS$Type)
str(newDS)

tr <- sample(1:nrow(newDS), nrow(newDS) * 0.7)

training_DS <- newDS[tr,]
training_DS <- subset(training_DS, select = -c(Perimeter))
str(training_DS)
head(training_DS)
testing_DS <- newDS[-tr,]
testing_DS <- subset(testing_DS, select = -c(Perimeter))
model <- glm(Type~., data = training_DS, family = "binomial")
summary(model)

prediction <- predict(model, type = "response")
predicted_class <- rep("No", nrow(training_DS))
predicted_class
predicted_class[prediction > 0.5] = "Yes"

table(predicted_class, training_DS$Type)

misclassificationRate <- (2 + 1) / (99 + 2 + 1 + 45)
misclassificationRate


## ROSA

newDSRosa <- wheatKernelDS
newDSRosa$Type[newDSRosa$Type != 2] = 0
newDSRosa$Type[newDSRosa$Type == 2] = 1

table(newDSRosa$Type)
newDSRosa$Type <- as.factor(newDSRosa$Type)
str(newDSRosa)


tr <- sample(1:nrow(newDSRosa), nrow(newDSRosa) * 0.7)
training_DS_Rosa <- newDSRosa[tr,]
testing_DS_Rosa <- newDSRosa[-tr,]

model_rosa <- glm(Type~Area+Perimeter+Compactness+Length_of_Kernel+Width_of_Kernel
                  , training_DS_Rosa, family = binomial)
summary(model_rosa)

prediction <- predict(model_rosa, newdata = testing_DS_Rosa, type = "response")
predicted_class <- rep(0, nrow(testing_DS_Rosa))
predicted_class
predicted_class[prediction > 0.5] = 1

val <- table(predicted_class, testing_DS_Rosa$Type)
val
misclassification_rate <- (val[1,2] + val[2,1]) / sum(val)
misclassification_rate

## whole DS

new_data <- wheatKernelDS[wheatKernelDS$Type != 3,]
str(new_data)
View(new_data)

pairs(new_data[,1:(length(new_data) - 1)], panel = panel.smooth, col = c(3,4,7))

new_data$Type <- as.factor(new_data$Type)

attach(new_data)
tr <- sample(1:nrow(newDSRosa), nrow(newDSRosa) * 0.7)
training_DS_Rosa <- newDSRosa[tr,]
testing_DS_Rosa <- newDSRosa[-tr,]

model_full <- glm(Type~Area+Length_of_Kernel+Length_of_Kernel_Groove, new_data, family = binomial)
summary(model_full)


############################################################################################
#   Full Data set Model
#
############################################################################################
ds <- wheatKernelDS
str(ds)
ds$Type <- as.factor(ds$Type)
attach(ds)
mod <- glm(Type~., data = ds, family = binomial)
summary(mod)


############################################################################################
#   GLM Model with Kama with full Data set
#
############################################################################################

set.seed(10)
n_ds <- wheatKernelDS
n_ds$Type[n_ds$Type != 1] = 0
n_ds$Type <- as.factor(n_ds$Type)
str(n_ds)
table(n_ds$Type)
attach(n_ds)
model_f <- glm(Type~., data = n_ds, family = binomial)
summary(model_f)

############################################################################################
#   GLM Model with Kama with training and testing DS
#
############################################################################################

tr <- sample(1:nrow(n_ds), nrow(n_ds) * 0.7)
tr_ds <- n_ds[tr,]
te_ds <- n_ds[-tr,]
attach(tr_ds)
model_tr <- glm(Type~., data = tr_ds, family = binomial)
summary(model_tr)

model_tr1 <- glm(Type~Area+Perimeter+Compactness+Assymetry_coefficient+Length_of_Kernel_Groove, 
                 data = tr_ds, family = binomial)

summary(model_tr1)

prediction <- predict(model_tr1, newdata = te_ds, type="response")
pred_class <- rep(0, nrow(te_ds))
pred_class[prediction > 0.5] = 1

ms_matrix <- table(pred_class, te_ds$Type)

misclassification_rate <- (ms_matrix[1,2] + ms_matrix[2,1]) / sum(ms_matrix)
misclassification_rate


############################################################################################
#   GLM Model with Rosa with full DS
#
############################################################################################
set.seed(10)
n_ds_r1 <- subset(wheatKernelDS, select = -c(Type))
n_ds_r1$T <- rep(0, nrow(n_ds_r1))
n_ds_r1$T[wheatKernelDS$Type == 2] = 1
n_ds_r1$T <- as.factor(n_ds_r1$T)
str(n_ds_r1)
table(n_ds_r1$T)
attach(n_ds_r1)

pairs(n_ds_r1, panel = panel.smooth, col = c(3,4,7))
cor(n_ds_r1[,1:(length(n_ds_r1) - 1)])


model_f_r1 <- glm(T~Area+Perimeter+Width_of_Kernel+Compactness+Length_of_Kernel_Groove+Assymetry_coefficient,
                  data = n_ds_r1, family = binomial)
summary(model_f_r1)

prediction <- predict(model_f_r1, type="response")
pred_class <- rep(0, nrow(n_ds_r1))
pred_class[prediction > 0.5] = 1

ms_matrix <- table(pred_class, n_ds_r1$T)

misclassification_rate <- (ms_matrix[1,2] + ms_matrix[2,1]) / sum(ms_matrix)
misclassification_rate

############################################################################################
#   GLM Model with Canadian with full DS
#
############################################################################################
set.seed(10)
n_ds_c <- wheatKernelDS
rows <- sample(nrow(n_ds_c))
n_ds_c <- n_ds_c[rows,]

n_ds_c$Type[n_ds_c$Type != 3] <-  0
n_ds_c$Type[n_ds_c$Type == 3] <- 1
n_ds_c$Type <- as.factor(n_ds_c$Type)

str(n_ds_c)
table(n_ds_c$Type)
attach(n_ds_c)

pairs(n_ds_c[,1:(length(n_ds_c) - 1)], panel = panel.smooth, col = c(3,4,7))
cor(n_ds_c[,1:(length(n_ds_c) - 1)])


model_c <- glm(Type~Perimeter+Compactness+Length_of_Kernel+Width_of_Kernel+Length_of_Kernel_Groove, 
                  data = n_ds_c, family = binomial)
summary(model_c)

prediction <- predict(model_c, type="response")
pred_class <- rep(0, nrow(n_ds_c))
pred_class[prediction > 0.5] = 1

ms_matrix <- table(pred_class, n_ds_c$T)

misclassification_rate <- (ms_matrix[1,2] + ms_matrix[2,1]) / sum(ms_matrix)
misclassification_rate

############################################################################################
#   GLM Model with full types
#
############################################################################################
set.seed(10)
n_ds_ft <- wheatKernelDS
n_ds_ft$Type <- as.factor(n_ds_ft$Type)

str(n_ds_ft)
table(n_ds_ft$Type)
attach(n_ds_ft)

pairs(n_ds_c[,1:(length(n_ds_c) - 1)], panel = panel.smooth, col = c(3,4,7))
cor(n_ds_c[,1:(length(n_ds_c) - 1)])


model_ft <- glm(Type~., 
               data = n_ds_ft, family = binomial)
summary(model_ft)

prediction <- predict(model_ft, type="response")

library(caret)
confusionMatrix(prediction, n_ds_ft$Type)


pred_class <- rep(0, nrow(n_ds_ft))

pred_class[prediction > 0.5] = 1

ms_matrix <- table(pred_class, n_ds_ft$Type)

misclassification_rate <- (ms_matrix[1,2] + ms_matrix[2,1]) / sum(ms_matrix)
misclassification_rate


############################################################################################
#   Pairs as per type
#   Corr as per type
############################################################################################

kama <- subset(wheatKernelDS, subset = c(Type == 1), select = -c(Type))
head(kama)
pairs(kama, panel = panel.smooth, col = c(3))
cor(kama)

rosa <- subset(wheatKernelDS, subset = c(Type == 2), select = -c(Type))
head(rosa)
pairs(rosa, panel = panel.smooth, col = c(4))
cor(rosa)


canadian <- subset(wheatKernelDS, subset = c(Type == 3), select = -c(Type))
head(canadian)
pairs(canadian, panel = panel.smooth, col = c(7))
cor(canadian)


### SVM

## Linear
tune_linear_training <- tune(svm, is_kama~., data = wheat_training, 
                             kernel = "linear", 
                             ranges = list(cost = c(0.001, 0.01, 0.1, 1, 10, 100, 1000)
                             ))
summary(tune_linear_training)

best_svm_linear_tr <- tune_linear_training$best.model ## it will give the best model
summary(best_svm_linear_tr)

## Prediction
pred1 <- predict(best_svm_linear_tr, newdata = wheat_testing)

# now for misclassification rate, construct misclassification table
misclassification_table <- table(pred1, Observed = wheat_testing[, "is_kama"])
misclassification_table

misclassification_rate <- (misclassification_table[1,2] + misclassification_table[2,1]) / sum(misclassification_table)
misclassification_rate


## Decision tree

class_tree_full <- tree(is_kama~., data = wheat_modified)
# . commadning R to use all the variables
summary(class_tree_full)
plot(class_tree_full)
text(class_tree_full,pretty = 0)

class_tree_training <- tree(is_kama~., data = wheat_training)
summary(class_tree_training)
plot(class_tree_training)
text(class_tree_training, pretty = 0)

pred_class_test1 <- predict(class_tree_training, newdata = wheat_testing, type = "class")
head(pred_class_test1)

observed_test <- wheat_testing[, "is_kama"]

misclassification_matrix <- table(pred_class_test1, observed_test)
misclassification_matrix

misclassrate <- ( misclassification_matrix[1,2] + misclassification_matrix[2,1] )/ sum(misclassification_matrix)

misclassrate

cv_classtree <- cv.tree(class_tree_training, FUN = prune.misclass) ## we give this function for classification problem,
# else we set K value
cv_classtree
plot(cv_classtree$size, cv_classtree$dev, type = "b", xlab = "Tree Size", 
     ylab = "Number of Misclassifications")

pruned_classtree <- prune.misclass(class_tree_training, best = 4)
plot(pruned_classtree)
text(pruned_classtree, pretty = 0)


# i) Test the model accuracy.

pred_class_test2 <- predict(pruned_classtree, newdata = wheat_testing, type = "class")
head(pred_class_test2)

observed_test <- wheat_testing[, "is_kama"]

misclassification_matrix_pruned <- table(pred_class_test2, observed_test)
misclassification_matrix_pruned

misclassrate <- ( misclassification_matrix_pruned[1,2] + misclassification_matrix_pruned[2,1] )/ sum(misclassification_matrix_pruned)

misclassrate
# j) Describe the terminal nodes of the resulting decision tree.

# explain the tree
pruned_classtree

