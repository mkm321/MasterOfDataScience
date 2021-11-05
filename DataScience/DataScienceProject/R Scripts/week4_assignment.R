library(ISLR)
attach(Default)
head(Default)
dim(Default)
str(Default)

model1 <- glm(default~balance, data = Default, family = binomial) ## so in our response variable we have only 2 values i.e.
# yes or no, so that's why we have defined the family here as binomial. If it has more than 2 variables, then we should
# use some other family than binomial.
summary(model1)
# deviance residuals - just a residuals, we expect it to be distributed around 0.
# in this this is good, as median is close to 0 and everything is distribtued aroun 0, which is good.
# if we want to compare 2 models, we can use AIC Value. Usually AIC Value is lower.
# but in this we are going to see misclassifications. we should expect the misclassification rate to be lower.
balance = 500
t = -10.65 + (0.005499 * balance) # this is our exponential's superscript.
# probability
probabilityOfDefault = exp(t) / (1 + exp(t))
probabilityOfDefault

model2 <- glm(default~balance+student+income, data = Default, family = binomial)
summary(model2)

heartDS <- read.csv("datasets/heart.csv")
attach(heartDS)
head(heartDS)
dim(heartDS)
str(heartDS)

heartDS$AHD = as.factor(heartDS$AHD)

model12 <- glm(AHD~., data = heartDS, family = binomial)
summary(model12)
model3 <- glm(AHD~Sex+ChestPain+Ca, data = heartDS, family = binomial)
summary(model3)
## by looking at above summary, we can see that Sex variable has a baseline category as 0 
## and ChestPain has a baseline category as asymptomatic. Baseline categories are not
## included in the model.

## so with this if chest pain value is equal to baseline value asymptomatic then in our model equation
## all of the values of ChestPain will be zero. i.e. B2 * 0 + B3 *0 + B4 * 0


attach(Smarket)
head(Smarket)
dim(Smarket)
str(Smarket)

model4 <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Smarket, family = binomial)
summary(model4)

# so now first for up we need to check if model has a dummy variable with 1 for "Up", to do that we can use
contrasts(Direction)
# now by looking at the coefficents we can create probability of Up with our equation
coef(model4)


## we are interested in misclassification rates in the logistic/classification regression model. 
## So that's why we need to create misclassification table first or otherwise it is generally 
## known as confusion matrix.

## Confusion matrix for a binary classification model is predicted + and - vs actual + and -
## to construct it we need to have a predicted class

predicted_prob <- predict(model4, type = "response")
predicted_prob
predicted_class <- rep("Down", nrow(Smarket))
predicted_class
predicted_class[predicted_prob > 0.5] = "Up"

table(predicted_class, Direction)

misclassificationRate <- (141 + 457) / (145 + 141 + 457 + 507)
misclassificationRate
falsePositiveRate <- 457 / (145 + 457)
falsePositiveRate
falseNegativeRate <- 141 / (141 + 507)
falseNegativeRate
