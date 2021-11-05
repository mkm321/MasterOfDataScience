# Load the data
AdvertisingDS <- read.csv("datasets/advertising.csv")
attach(AdvertisingDS)
head(AdvertisingDS)
dim(AdvertisingDS)
sapply(AdvertisingDS, class)

# cor and cov matrix
pairs(AdvertisingDS, panel = panel.smooth)
cor(AdvertisingDS)
cov(AdvertisingDS)

# model and least square estimates
model1 <- lm(Sales~TV+Radio+Newspaper)
summary(model1)

# Significance - Hypothesis testing

# since advertisement is not so significant, new model is
model2 <- lm(Sales~TV+Radio)
summary(model2)
# Access the overall accuracy
anova(model2)
qf(0.95,3,196)
fittedval <- fitted(model2)

# Calc predicted values and residuals
pred_values <- predict(model2)
resid_value <- resid(model2)
pred_values
resid_value
# plot the resid vs pred
plot(pred_values, resid_value, xlab="Fitted Values", ylab="Residual Values")
# plot the hist of resiudal
hist(resid_value, xlab = "Residuals", main="Histogram of Residuals")
# residual plot
par(mfrow=c(2,2))
plot(model2)
# use the multivariate model for prediction
predict(model2, as.data.frame(cbind(TV = 400, Radio = 60)))

## non-linear models
model3 <- lm(Sales~TV+Radio+TV*Radio)
summary(model3)

## polynomial regression
model4 <- lm(Sales~TV+I(TV*TV)+I(TV*TV*TV))
summary(model4)

## resulting selected model is model3
