# Card piles
# This is crude. How can we effectively measure order???
# The approach here is very simplistic and so is just
# intended to get a feel.
x1 <- sample(1:52)
x2 <- sample(1:52)

# x1 <- x2 with this commented out, x1 and x2 are different, otherwise identical

cs <- sum((x1 - x2)^2)

# Simulate what is supposedly random
d <- replicate(1000,
               {
                 a <- sample(1:52)
                 b <- sample(1:52)
                 
                 sum((a - b)^2)
               })

hist(d)
abline(v = cs, col = 2, lwd = 2, lty = 2)


# maternal smoking data set
birthwt <- read.csv('birthwt.csv')

delta <- aggregate(bwt ~ smoke, birthwt, mean)$bwt
delta


# H0: mu1 == mu2
# H1: mu1 < mu2
#
# cv = 0.05
#
# mu1 -> smokers
# mu2 -> non smokers
#
# mu1 - mu2 < 0
# mu1 < mu2
#
# or
# mu2 - mu1 > 0
# so interested in a difference greater than zero

delta <- -diff(delta)

# Simulation of H0
d <- replicate(1000,
               {
                 smoke.shuffle <- sample(birthwt$smoke)
                 delta <- aggregate(bwt ~ smoke.shuffle,
                                    birthwt, mean)$bwt
                 delta <- -diff(delta)
               })

mean(d)
range(d)

hist(d, main = '', col = 'skyblue', xlim = c(-1, 1) * 300)
abline(v = delta, col = 2, lwd = 2, lty = 2)

# One sided test, so p-value calculation uses one side of the distribution
# The +ve side is used since mu2 - mu1 > 0
pVal <- mean(d > delta)
