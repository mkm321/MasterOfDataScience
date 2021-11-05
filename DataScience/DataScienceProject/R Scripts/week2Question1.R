## Question 1
## part(a)

Advertising <- read.csv("datasets/advertising.csv")
head(Advertising)
attach(Advertising)
dim(Advertising)
sapply(Advertising, mean)
sapply(Advertising, var)
sapply(Advertising, class)

## visualisation part(b)

## one of the assumption is variance should be constant but in this case 
# it is not.
plot(Sales~TV)
plot(Sales, TV)
par(mfrow=c(1,3))
plot(Sales~TV)
plot(Sales~Newspaper)
plot(Sales~Radio)

### part(c)

# find the correlation coefficient to get the .
# 0.7 is relationship is somewhat strong but not all strong.
# since it is positive there is a relation
# if it is close to 1 there is very strong relationship
# if it is close to -1 then there is very strong negative relationship

cor(Sales, TV)

### part(d)
# find the least square estimates 

model1 <- lm(Sales~TV)
## Population data set is very large dataset for all the data in the world
## and the data set we have its just a small set of that.

# this will give the model summary
# model equation E(Y) = b0 + b1(x) -  so in this case Y is Sales and x is TV
# now we need to find our the b0^ and b1^
# in this summary - 7.03 is b0^ and 0.04 is b1^
# so now E(Sales) = 7.03 + 0.047 * TV
# so these two are the least square estimates of the model
# Std. Error is error of least square estimates for example how much it will be different from
# actual population problem. The actual population parameter will be 7.03 - 0.45 which is b0 
# and 0.04 - 0.002 will be the value for b1

summary(model1)

### part(e)
# access the accuracy of the part of parameters
# smaller the error better the coefficients
# for this we can use Std. Error and the confidence interval for the coefficients.

confint(model1)

### part(f)
# Test the significance of the slope for LM,
# for this write down the hypothesis and then based on p values/confidence interval
# you have to comment.

### part(g)
# plot the straightline in the scatterplot and comment.
plot(Sales~TV)
abline(c(7.032594, 0.047537), col="red")

### part(h)
# Access the overall accuracy of the modek
# we can use
# Residual standard error
# R^2 value
# p-value

# now for this residual std. error is 3.259, so
# so if y^ is E(Sales) = 7.03 + 0.04TV
# then actual y should be around y^ +- 3.259

summary(Sales)
# so now based on summary, we can check and confirm as per the values
# for instance, for median it is 12.90 and 12.90 +- 3.259 seems to be high.

# for R squared in Summary of model, 61.19 % is the proportion of variable explained by the model.
# usually we expect it to be high for a good model
## we expect Resiudal to be low
## R squared to be high
## p-value to be low
## for a good model.

#### ASUMPTIONS : 
# 1. LINEARITY
# 2. NORMALITY (NORMAL DISTRIBUTION)
# 3. CONSTANT VARIANCE ASSUMPTION ( HOMOSCEDASTICITY)
# 4.

par(mfrow=c(2,2))
plot(model1)

# For Linearity
# if it is valid then the plot for Residual vs fitted should be scattered/random and there should not
# be any patter, It should be like stars in the sky.
# since we can see pattern here then we can say that Linearity Assumption is not valid
# and same thing should reflect in scale vs Fitted plot.

# For Homoscedasicity
# here also we can see(Resiudal vs fitted) the spread is not constant, 
# therefore this assumption is also not valid.


# for Normality
# we do not have all points lied on the straight line in Normal vs Theoretical plot
# therefore, this assumption is also not valid

# so now if the Assumptions are not valid, then there is something to improve the model.
# and something to look more.

### part(i)
# if we are happy with our model and assumptions
# we can use predict
plot(Sales~TV)
predict(model1, list(TV=400))

# when the TV value is 100, then we can predict the Sales to be 11.78