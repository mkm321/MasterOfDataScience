## Classification problem by using SVMs

## Data Explore
library(ISLR)
attach(Auto)
dim(Auto)
head(Auto)
str(Auto)
summary(Auto)

## 2. Use support vector approaches in order to predict whether a given car gets
## high or low gas mileage based on the “Auto” data set.
hist(mpg)

## a. Create a binary variable that takes “1” for cars with gas mileage above 
## median and “0” for cars with gas mileage below the median.

mpg_median <- median(mpg)
mpg_code <- rep(0, nrow(Auto))
mpg_code[mpg > mpg_median] = 1
mpg_code

str(mpg_code)
## so this is numerical but we want a factor variable
mpg_code <- as.factor(mpg_code)
str(mpg_code)

## Combine the data

## now we do't want name column because we aren't using it so removing it

Auto <- data.frame(Auto[, c(-1,-9)], mpg_code)
head(Auto)

## part b)
library(e1071)
## first we will do it with full data set but then I need to complete it by dividing
## the data set into training and testing data set

## with full Data set
## we have difference in variance for the columns, for example individual variance
## of cyllinder is low and the displacement is large because of data is large in disp
## So, this won't affect our SVM, therefore we do it scale = TRUE

## cost is the tolerance/ width of the margin
svm_fit1 <- svm(mpg_code~., data = Auto, kernel = "linear", scale = TRUE, cost = 1)  
summary(svm_fit1)

## by looking at the summart we have feed the 1st classification model and the boundary
## is linear and cost is 1 which control the width of the margin, for all the observation
## that lie on the margin or in the margin are called support vectors, so we are given
## 43 and 45 that is 43 belongs to 0 class and 45 belongs to 1 class.

## if the cost is large, our margin gets narrow then number of support vector will be less.
## so here we have to decide how much cost will be the best. 

svm_fit1$index ## this shows which observation lies on and in the margin

## now we fit another model with high cost and see what happens

svm_fit2 <- svm(mpg_code~., data = Auto, kernel = "linear", scale = TRUE, cost = 100)
summary(svm_fit2)

## here we can see that only 81 observation that lie on and in the margin.

## now we reduce from 1
svm_fit3 <- svm(mpg_code~., data = Auto, kernel = "linear", scale = TRUE, cost = 0.01)
summary(svm_fit3)

## now we see how to decide the optimal value of the cost.

## to do that we have tune function, these are function to tune the parameter.

set.seed(5)
tune_out1 <- tune(svm, mpg_code~., data = Auto, kernel = "linear", 
                  ranges = list(cost = c(0.001, 0.01, 0.1, 1, 10, 100, 1000)
                  ))
summary(tune_out1)

## these are the result from the cross validation,

## cross validation - 10 fold by default
## it gives the best cost parameter, and least error ( best performance)

## what they expect is to select the best model from this output.


best_svm_linear <- tune_out1$best.model ## it will give the best model
best_svm_linear
summary(best_svm_linear)

plot(best_svm_linear, data = Auto)


## it is very hard for us to see with the plot when the variables are 3 and more than 3
## so here is 7 therefore, we can't visualise in this case.
## but if we have two variables, then we can just mention (plot(svm_fit_2_variables)) this is new svm fit
## with 2 variables.

# part c)

## Polynomial SVM

## Gamma is only for radial so here we are considering only degree

set.seed(5)
tune_out2 <- tune(svm, mpg_code~., data = Auto, kernel = "polynomial", 
                  ranges = list(
                    cost = c(0.001, 0.01, 0.1, 1, 10, 100, 1000),
                    degree = c(2:5)
                  ))
summary(tune_out2)

best_svm_polynomial <- tune_out2$best.model
best_svm_polynomial
summary(best_svm_polynomial)

## here coef.0 means 

## Radial SVMs

set.seed(5)
tune_out3 <- tune(svm, mpg_code~., data = Auto, kernel = "radial", 
                  ranges = list(
                    cost = c(0.001, 0.01, 0.1, 1, 10, 100, 1000),
                    gamma = c(0.5,1,2,3,5)
                  ))
summary(tune_out3)

best_svm_radial <- tune_out3$best.model
best_svm_radial
summary(best_svm_radial)


## which one is the best model to choose from linear, polynomial and radial

## we can find the error values for all of them through tuned results and then 
## we can compare which one has the lowest error.

######
## now split the data set into training and test, using seed 10


set.seed(10)
index <- sample(1: nrow(Auto), nrow(Auto) * 0.7)
training_data <- Auto[index,]
head(training_data)
testing_data <- Auto[-index,]
## Linear
tune_linear_training <- tune(svm, mpg_code~., data = training_data, kernel = "linear", 
                                          ranges = list(cost = c(0.001, 0.01, 0.1, 1, 10, 100, 1000)
                                          ))
summary(tune_linear_training)

best_svm_linear_tr <- tune_linear_training$best.model ## it will give the best model
summary(best_svm_linear_tr)

## Prediction
pred1 <- predict(best_svm_linear_tr, newdata = testing_data)

# now for misclassification rate, construct misclassification table
misclassification_table <- table(pred1, Observed = testing_data[, "mpg_code"])
misclassification_table

misclassification_rate <- (misclassification_table[1,2] + misclassification_table[2,1]) / sum(misclassification_table)
misclassification_rate

## Polynomial
tune_polynomial_training <- tune(svm, mpg_code~., data = training_data, kernel = "polynomial", 
                             ranges = list(cost = c(0.001, 0.01, 0.1, 1, 10, 100, 1000),
                                           degree = c(2:5)
                             ))
summary(tune_polynomial_training)

best_svm_polynomial_tr <- tune_polynomial_training$best.model ## it will give the best model
summary(best_svm_polynomial_tr)

## Prediction
pred2 <- predict(best_svm_polynomial_tr, newdata = testing_data)

# now for misclassification rate, construct misclassification table
misclassification_table <- table(pred2, Observed = testing_data[, "mpg_code"])
misclassification_table

misclassification_rate <- (misclassification_table[1,2] + misclassification_table[2,1]) / sum(misclassification_table)
misclassification_rate

## Radial

tune_radial_training <- tune(svm, mpg_code~., data = training_data, kernel = "radial", 
                                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 10, 100, 1000),
                                               gamma = c(0.5,1,2,3,5)
                                 ))
summary(tune_radial_training)

best_svm_radial_tr <- tune_radial_training$best.model ## it will give the best model
summary(best_svm_radial_tr)

## Prediction
pred3 <- predict(best_svm_radial_tr, newdata = testing_data)

# now for misclassification rate, construct misclassification table
misclassification_table <- table(pred3, Observed = testing_data[, "mpg_code"])
misclassification_table

misclassification_rate <- (misclassification_table[1,2] + misclassification_table[2,1]) / sum(misclassification_table)
misclassification_rate
