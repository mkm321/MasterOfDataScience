AutoDS <- read.csv("datasets/Auto1.csv")
attach(AutoDS)
head(AutoDS)
dim(AutoDS)
sapply(AutoDS, class)
AutoDS <- subset(AutoDS, select = c(mpg, cylinders, displacement, weight, acceleration, year, origin))

pairs(AutoDS, panel = panel.smooth)
cor(AutoDS)
cov(AutoDS)

model1 <- lm(mpg~cylinders+displacement+weight+acceleration+year+origin)
summary(model1)

model2 <- lm(mpg~displacement+weight+acceleration+year+origin)
summary(model2)

model3 <- lm(mpg~weight+year+origin)
summary(model3)
summary(mpg)

pred_values <- predict(model2)
pred_values

resid_value <- resid(model2)
resid_value

plot(pred_values, resid_value, xlab="Predicted Values", ylab="Residual Values")

hist(resid_value, xlab = "Residuals", main="Histogram of Residuals")

par(mfrow=c(2,2))
plot(model2)

predict(model2, as.data.frame(cbind(displacement = 500, weight = 1500, acceleration = 26, year = 85, origin = 3)))
