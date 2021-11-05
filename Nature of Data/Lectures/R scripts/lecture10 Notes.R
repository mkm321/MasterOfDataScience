# Slide 12
# QQ plot
#
# y is the data being considered evaluated
# x is the normally distributed data being used as a reference

n <- 10  # Try different sample sizes
y <- rnorm(n)
# y <- rexp(n)  # could try other distributions

# Order / sort the values
y <- sort(y)

# P(Z < zi) = i / (n + 1)
# x represents probabilities (0, 1)
# BUT zero and one are not included
x <- 1:length(y) / (length(y) + 1)

# Convert the x values (probabilities) to x axis locations
x <- qnorm(x)

# Our crude version of a QQ plot
plot(y ~ x)

# intercept of zero, slope of one
# crude, probability not the best fit
abline(coef = 0:1)

# built in functions to do this
# compare with above
qqnorm(y) # plot the QQ-plot
qqline(y) # add the line to the plot showing Normality

# better version
# An alternative is to use the car add-on library:
library(car)
qqPlot(y) # plot a QQ-plot with a line


# Slide 17
# Standardising data
s <- rnorm(100, 12, 7)
#s <- rexp(100, 10)  # try another distribution

mean(s); sd(s)  # not zero mean and not sd of 1

z <- (s - mean(s)) / sd(s)  # Standardise
mean(z); sd(z)  # now zero mean and sd = 1; approximately


# Slide 26
df <- read.csv('birthwt.csv')

aggregate(df$bwt, list(df$smoke), length)
aggregate(df$bwt, list(df$smoke), mean)
aggregate(df$bwt, list(df$smoke), sd)

# or
aggregate(bwt ~ smoke, df, sd)


# Assuming equal variances in the populations
t.test(df$bwt ~ df$smoke, var.equal = TRUE,
       alternative = 't')  # H1: mu1 <> mu2
t.test(df$bwt ~ df$smoke, var.equal = TRUE,
       alternative = 'l')  # H1: mu1 < mu2
t.test(df$bwt ~ df$smoke, var.equal = TRUE,
       alternative = 'g')  # H1: mu1 > mu2


# Slide 34
# Using a t distribution
# Getting x axis value (t statistic)
# for a probability of 2.5% and df of 10

# from the left
qt(0.025, 10)

# from the right, same absolute value due to symmetry
qt(1 - 0.025, 10)


# Slide 36
# Using a t distribution
# Finding the probability for a standardised
# t statistic and given df
#
# t =  -2.228; df = 10
pt(-2.228, 10) # 2.5% as expected
