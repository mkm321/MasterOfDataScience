# importing the Advertising dataset
Advertising <- read.csv("datasets/advertising.csv")
attach(Advertising)
# fetching first 6 enteries
head(Advertising)
# fetching names of the variables
names(Advertising)
# dimension of the dataset.
dim(Advertising)
# plotting sales vs TV scatterplot.
plot(Sales~TV)
## plotting 3 different plots or Comparing 3 different media types w.r.t sales.
# below will create 1 row and 3 columns for the plots
par(mfrow=c(1,3))
plot(Sales~TV, ylab = "Sales", xlab = "TV")
plot(Sales~Newspaper, ylab = "Sales", xlab = "Newspaper")
plot(Sales~Radio, ylab = "Sales", xlab="Radio")

## now to make sure we use correct scale for comparing different types.
# will add x and y limits

par(mfrow=c(1,3))
plot(Sales~TV, ylab = "Sales", xlab = "TV", xlim=c(0,300), ylim=c(-5,30))
plot(Sales~Newspaper, ylab = "Sales", xlab = "Newspaper", xlim=c(0,300), ylim=c(-5,30))
plot(Sales~Radio, ylab = "Sales", xlab="Radio", xlim=c(0,300), ylim=c(-5,30))

# fetching linear model between Sales and TV to find slope and intercept
lm(Sales~TV)
lm(Sales~Newspaper)
lm(Sales~Radio)
x
# now plotting the intercept and slope details in the above plot
par(mfrow=c(1,3))
plot(Sales~TV, ylab = "Sales", xlab = "TV")
abline(a=7.03259, b=0.04754)
plot(Sales~Newspaper, ylab = "Sales", xlab = "Newspaper")
abline(a=12.35141, b=0.05469)
plot(Sales~Radio, ylab = "Sales", xlab="Radio")
abline(a=9.3116, b=0.2025)

# fetching the summary of linear model
model = lm(Sales~TV)
summary(model)

# ANOVA - Analysis of Variance Table
anova(model)
# confidence interval
confint(model)
# predicting the model
predict(model,list(TV = 100.0))
# model checking plots
par(mfrow=c(2,2))
plot(model)
