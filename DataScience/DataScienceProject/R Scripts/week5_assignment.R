## Question 1 Cross Validation
#(a)
library(ISLR)
attach(Auto)
dim(Auto)
head(Auto)
str(Auto)
#(b)
# the euations are 
mpg = beta0 + beta1 * horsepower
mpg = beta0 + beta1 * horsepower + beta2 * horsepower^2
mpg = beta0 + beta1 * horsepower + beta2 * horsepower^2 + beta3 * horsepower^3


set.seed(2)
tr1 = sample(1:nrow(Auto), nrow(Auto) * 0.7)
length(tr1)
# splitting the training data
auto_training_data = Auto[tr1,]
# building models
model1 <- lm(mpg~horsepower, data = Auto, subset = tr1)
mse1 <- mean((mpg - predict(model1, Auto))[-tr1]^2)

model2 <- lm(mpg~poly(horsepower, 2), data = Auto, subset = tr1)
mse2 <- mean((mpg - predict(model2, Auto))[-tr1]^2)

model3 <- lm(mpg~poly(horsepower, 3), data = Auto, subset = tr1)
mse3 <- mean((mpg - predict(model3, Auto))[-tr1]^2)

mse_seed2 <- c(mse1, mse2, mse3)
order <- c(1,2,3)

plot(order, mse, type="b", col = 2)

## Now for seed 5
set.seed(5)
tr1 = sample(1:nrow(Auto), nrow(Auto) * 0.7)
length(tr1)
# splitting the training data
auto_training_data = Auto[tr1,]
# building models
model1 <- lm(mpg~horsepower, data = Auto, subset = tr1)
mse1 <- mean((mpg - predict(model1, Auto))[-tr1]^2)

model2 <- lm(mpg~poly(horsepower, 2), data = Auto, subset = tr1)
mse2 <- mean((mpg - predict(model2, Auto))[-tr1]^2)

model3 <- lm(mpg~poly(horsepower, 3), data = Auto, subset = tr1)
mse3 <- mean((mpg - predict(model3, Auto))[-tr1]^2)

mse_seed5 <- c(mse1, mse2, mse3)


## Now for seed 8
set.seed(8)
tr1 = sample(1:nrow(Auto), nrow(Auto) * 0.7)
length(tr1)
# splitting the training data
auto_training_data = Auto[tr1,]
# building models
model1 <- lm(mpg~horsepower, data = Auto, subset = tr1)
mse1 <- mean((mpg - predict(model1, Auto))[-tr1]^2)

model2 <- lm(mpg~poly(horsepower, 2), data = Auto, subset = tr1)
mse2 <- mean((mpg - predict(model2, Auto))[-tr1]^2)

model3 <- lm(mpg~poly(horsepower, 3), data = Auto, subset = tr1)
mse3 <- mean((mpg - predict(model3, Auto))[-tr1]^2)

mse_seed8 <- c(mse1, mse2, mse3)


order <- c(1,2,3)
plot(order, mse_seed2, type="b", col = "red")
lines(mse_seed5, type = "b", col = "blue")
lines(mse_seed8, type = "b", col = "green")
legend(2.5, 25, legend=c("Seed 2", "Seed 5", "Seed 8"),
       col=c("red", "blue", "green"),lty = 1)
# By looking at the result we can see mse2 has the least error therefore,
# model2 looks the best model

# By setting the seed to 2,5 and 8, we can see that 
### Drawback for Validation Set Approach - 
# when we use different training set/ split we gets different values of our MSE values.

### Using LOOCV
library(boot)
poly_order <- c(1:10)
cv_error <- rep(0,10)
for(i in poly_order){
  m <- glm(mpg~poly(horsepower, i), data = Auto)
  cv_error[i] <- cv.glm(Auto, m)$delta[1]
}

cv_error
plot(poly_order, cv_error, type = "b", xlab = "Order of Polynomials", 
     col = "red", ylab = "LOOCV Error")
# from the plot, we can see that there is a significant drop of error for order 2
# and for the rest of the order there are slight increase and decrease for the error.
# For order 7, the error is the lowest but there is no quite significant improvement in the error 
# as compared to order 2.
# so we can use order 2 to make our model less complex and hence this is the best model.

### Using K-Fold CV

cv_error_K_fold <- rep(0,10)
for(i in 1:10){
  mod <- glm(mpg~poly(horsepower, i), data = Auto)
  cv_error_K_fold[i] <- cv.glm(Auto, mod, K = 10)$delta[1]
}
cv_error_K_fold

plot(poly_order, cv_error_K_fold, type = "b", xlab = "Order of Polynomials", 
     col = "red", ylab = "10-Fold CV Error")

# So, here we will also choose 2.
## so 10, fold is computationaly fast as compared LOOCV.

## Question 2 Cross Validation

Advertising <- read.csv("datasets/advertising.csv")
dim(Advertising)
attach(Advertising)
str(Advertising)
head(Advertising)

Sales = beta0 + beta1 * TV
Sales = beta0 + beta1 * Radio
Sales = beta0 + beta1 * Newspaper

model1 <- glm(Sales~TV, data = Advertising)
model2 <- glm(Sales~Radio, data = Advertising)
model3 <- glm(Sales~Newspaper, data = Advertising)
model4 <- glm(Sales~TV+Radio, data = Advertising)
model5 <- glm(Sales~TV+Newspaper, data = Advertising)
model6 <- glm(Sales~Radio+Newspaper, data = Advertising)
model7 <- glm(Sales~TV+Radio+Newspaper, data = Advertising)

cv_error_Sales <- rep(0,7)
cv_error_Sales[1] = cv.glm(Advertising, model1, K = 10)$delta[1]
cv_error_Sales[2] = cv.glm(Advertising, model2, K = 10)$delta[1]
cv_error_Sales[3] = cv.glm(Advertising, model3, K = 10)$delta[1]
cv_error_Sales[4] = cv.glm(Advertising, model4, K = 10)$delta[1]
cv_error_Sales[5] = cv.glm(Advertising, model5, K = 10)$delta[1]
cv_error_Sales[6] = cv.glm(Advertising, model6, K = 10)$delta[1]
cv_error_Sales[7] = cv.glm(Advertising, model7, K = 10)$delta[1]

plot(1:7, cv_error_Sales, type = "b", xlab = "Order of Polynomials", 
     col = "red", ylab = "10-Fold CV Error for Sales")
# By looking at this graph, the best model is TV + Radio because it has lowest error.
# TV + Radio + Newspaper has also the lowest error but it is not much signficantly lower

## Ques 3
# 
# a
set.seed(100)
x = 10*rexp(20)

#b
mean(x)

#c
count_BS <- 1000
# sample(x, replace = TRUE)
mean_samples <- rep(0,count_BS)
mean_samples[1] <- mean(x)

for(i in 2:count_BS) {
  mean_samples[i] <- mean(sample(x, replace = TRUE))
}

mean_samples

#d

hist(mean_samples, main = "Mean of 1000 bootstrapped samples", xlab = "Mean samples")
