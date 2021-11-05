Auto <- read.csv("datasets/auto.csv")
head(Auto)
attach(Auto)
dim(Auto)
sapply(Auto, mean)
sapply(Auto, var)
sapply(Auto, class)

par(mfrow=c(1,3))
plot(mpg~displacement)
plot(mpg~weight)
plot(mpg~acceleration)

cor(mpg, weight)
model2 <- lm(mpg~weight)
summary(model2)

confint(model2)
plot(mpg~weight)
abline(c(46.3173992, -0.0076766), col="red")
summary(mpg)

par(mfrow=c(2,2))
plot(model2)

predict(model2, list(weight=1300))
