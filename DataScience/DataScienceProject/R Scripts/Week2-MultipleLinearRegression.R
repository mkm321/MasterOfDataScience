## Multiple Linear Regression for Advertising data set.
Advertising <- read.csv("datasets/advertising.csv")
head(Advertising)
attach(Advertising)
sapply(Advertising, mean)
dim(Advertising)

## covariance and correlation matrices for Sales vs TV, Radio and Newspaper.

pairs(Advertising, panel = panel.smooth)
# From this we can say that Sales vs TV depicts a strong linear relationship.
# Sales vs Radio is also depicting a strong linear relationship.
# but, Sales vs Newspaper depicting a weak linear relationship.
cor(Advertising) # correlation matrix for advertisiment
# frpm this we can see that Correlation between Sales and TV are the highest,
# while Sales and Newspaper is lowest.
cov(Advertising) # covariance matrix

## Construct the multiple linear regression model and find the least square 
## estimates of the model parameters.

MLRModel <- lm(Sales~TV+Radio+Newspaper)
summary(MLRModel)

## By looking at the summary of this multiple linear regression model,
# we can see that  B0 hat - 2.938889, B1 hat - 0.045765, B2 hat - 0.188530 and
# B3 hat - -0.001037

# from this our equation should be - Y(Sales hat) = 2.938889 + 0.045765 TV + 0.188530 Radio - 0.001037 Newspaper

## Test the significance of the parameters and find the resulting model to model
## Sales in terms of advertising modes, TV, Radio and Newspaper

# Here we will use Hypothesis testing. H0: Bi = 0 vs H1: Bi != 0
# From the summary output we can see that for 5% level of significance, value for
# TV is less than 0.05, thus there is enough evidence to state that we can reject the
# null hypothesis H0 at 5% level of significance. Also at 1% level of significance,
# p-value for TV is still less than 0.01, thus stating the strong linear relationship.

# similary, for Radio we can also see that there is enough evidence to reject the null hypothesis
# H0 and is establishinh high linear relationship

# However, for newspaper, we can see that at 5% level of signinficance, p-value
# is 0.86 which is not less than 0.05. Thus we can reject the alternate hypothesis here
# meaning we can ignore newspaper.

# thus our model will become:
updatedMLRModel <- lm(Sales~TV+Radio)
summary(updatedMLRModel)

## (e) Assess the overall accuracy of the model.
anova(updatedMLRModel)
qf(0.95,2,197)
