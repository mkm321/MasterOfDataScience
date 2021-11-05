# Wilcoxon-Mann-Whitney test

orange <- c(6, 10, 14, 15, 16)
green <- c(8, 9, 11, 12, 13)

plot(orange, rep(1, length(orange)),
     col = 'orange', pch = 20, cex = 2)
points(green, rep(1, length(green)),
       col = 'green', pch = 20, cex = 2)

sum(outer(green, orange, '<'))
sum(outer(green, orange, '>'))

length(orange) * length(green)


df <- read.csv('birthwt.csv')
head(df)

# For subset
t <- df[1:6, ]

no <- subset(t, t$smoke == 'no')
yes <- subset(t, t$smoke == 'yes')

# [0, 8] min and max U for this data

sum(outer(yes$bwt, no$bwt, '<'))
sum(outer(no$bwt, yes$bwt, '<'))

t[order(t$bwt), ]

wilcox.test(t$bwt ~ t$smoke)
wilcox.test(no$bwt, yes$bwt)


# Complete data set
no <- subset(df, df$smoke == 'no')
yes <- subset(df, df$smoke == 'yes')

# min and max for U statistics
x <- c(0, nrow(no) * nrow(yes))
x
# Mid point of U, hence no group difference
x[2] / 2

sum(outer(yes$bwt, no$bwt, '<'))
sum(outer(no$bwt, yes$bwt, '<'))

wilcox.test(df$bwt ~ df$smoke)
wilcox.test(no$bwt, yes$bwt)


# Confidence intervals
# The luxury of a known populations; use normal distribution
x <- rnorm(50, 15)	# actual pop mean = 15
y <- rnorm(50, 10)	# actual pop mean = 10
					# difference in means is actually 5
					# but we pretend we don't know that!

wilcox.test(x, y)                     # Are they different; absolutely
wilcox.test(x, y, alternative = 'l')  # Is x < y, no way
wilcox.test(x, y, alternative = 'g')  # Is x > y, absolutely

# Generate estimate of population difference in means
p <- mean(x) - mean(y)
p

# Determine a confidence interval for the difference between x and y
# Generate confidence interval for true difference in population means
d <- replicate(10000,
               {
                 ix <- sample(1:length(x), replace = TRUE)
                 iy <- sample(1:length(y), replace = TRUE)
                 
                 mean(x[ix]) - mean(y[iy])
               })

hist(d)
abline(v = 5, col = 2, lwd = 2, lty = 2)

# calculate 95% confidence interval
q <- quantile(d, c(0.025, 0.975))
q

abline(v = q, col = 'purple', lwd = 3, lty = 3)

# Confidence interval for maternal smoking dataset
df <- read.csv('birthwt.csv')

# Extract entire data set into the two groups
no <- subset(df$bwt, df$smoke == 'no')
yes <- subset(df$bwt, df$smoke == 'yes')

# Difference means for the sample;
# estimate of difference in population
res <- mean(no) - mean(yes)

# resample using "boostrapping"
d <- replicate(1000,
               {
					# bootstrapping means doing the following:
					#  - create new sample of same size as original
					#  - must use replacement;
					#    otherwise we generate a shuffled original!
					#  - must preserve group sizes; 472 no smoke, 484 smoke
					#    so doing the two samples! Don't want to induce differences
					ns <- sample(no, replace = TRUE)
					s <- sample(yes, replace = TRUE)
                 
					# difference in means
					mean(ns) - mean(s)
               })

# find boundaries for central 95% of the data
q <- quantile(d, c(0.025, 0.975))
q     # conf interval, range within which true pop. difference is expected to reside


# visualise as matter of interest
hist(d,
	 main = 'Confidence Interval\nfor\ndifference in means',
	 xlab = 'Difference in means')
abline(v = res, col = 'blue', lwd = 2) # point estimate of pop mean
abline(v = q, col = 'purple', lwd = 2, lty = 3) # show confidence interval



# Mapping disease: Kidney Cancer in US

# Simulated disease rates
# We see minimum and maximum usually happens in smaller population
set.seed(12345)

popn = rep(c(100, 1000), each=5)

for(i in 1:10)
{
  cnt = rbinom(length(popn), popn, 0.1)
  rate = cnt / popn
  names(rate) = LETTERS[1:10]
  
  cols = rep("grey", 10)
  cols[which.max(rate)] = "red"
  cols[which.min(rate)] = "blue"
  
  barplot(rate, ylim=c(0,0.2), col=cols)
  abline(v = 6.1, , col = 'purple', lwd = 3)
  
  Sys.sleep(1)   # One second sleep / pause
}

# Simulated average disease rates
# Here we ignore pop. size, so there is no rate difference
trials <- 10000
theRate <- vector('double', length(popn))

for(i in 1:trials)
{
  cnt <- rbinom(length(popn), popn, 0.1)
  rate <- cnt / popn

  theRate <- theRate + rate
}

names(theRate) <- LETTERS[1:10]
barplot(theRate / trials)


# Bootstrap binomial confidence intervals for true rate in pop.
# Method 1
germinate <- 1
notGerminate <- 0

seeds <- c(rep(germinate, 15),
           rep(notGerminate, 5))

d <- replicate(1000,
               {
                 res <- sample(seeds, replace = TRUE)
                 mean(res)
               })
mean(d)
q <- quantile(d, c(0.025, 0.975))
q

# use of expression allows showing Greek letter
hist(d, col = 'skyblue', xlab = expression(rho),
     main = 'Proportion of seeds\nthat germinate')
abline(v = q, col = 2, lwd = 2, lty = 2)

# Method 2
d <- rbinom(1000, size = 20, prob = 15/20)
d <- d / 20

mean(d)
q <- quantile(d, c(0.025, 0.975))
q

hist(d, col = 'skyblue', xlab = expression(rho),
     main = 'Proportion of seeds\nthat germinate')
abline(v = q, col = 2, lwd = 2, lty = 2)


# Calculate Poisson confidence interval for true pop. lambda / expected death rate
horsekick <- c(109, 65, 22, 3, 1)
deaths <- rep(0:4, horsekick)

d <- replicate(1000,
               {
                 res <- sample(deaths, replace = TRUE)
                 mean(res)
               })
mean(d)
q <- quantile(d, c(0.025, 0.975))
q

hist(d, col = 'skyblue', xlab = expression(rho),
     main = 'Proportion of deaths\nby horse kick')
abline(v = q, col = 2, lwd = 2, lty = 2)
