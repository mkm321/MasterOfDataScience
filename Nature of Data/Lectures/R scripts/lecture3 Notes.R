# Using the R functions
lambda <- 0.61
dpois(0:4, lambda)


# Continuity correction
p <- 0.5
bins <- 0:10

d <- dbinom(bins, 10, p)

myCols <- c(rep('green', 7), rep('skyblue', 4))
t <- barplot(d, names.arg = bins, main = 'Continuity correction',
             ylim = c(0, 0.3), col = myCols)

mu <- 10 * p
sigma <- sqrt(mu * (1 - p))

curve(dnorm(x, mu + 1.7, sigma),
      -3, 15, add = TRUE, col = 'red', lwd = 2)
abline(v = 7.9, col = 'purple', lwd = 2, lty = 2)

pbinom(6, 10, p)
pnorm(6 + 0.5, mu, sigma)

pbinom(5, 10, p)
pnorm(6 - 0.5, mu, sigma)
pnorm(5 + 0.5, mu, sigma)


# Examples of normal approximation to binomial
#
# Z = (X - mu) / sigma

n <- c(5, 10, 100)
x <- floor(n * 0.6)
p <- 0.5

mu <- n * p
sigma <- sqrt(mu * (1 - p))

pb <- 1 - pbinom(x, n, p)
pnUC <- 1 - pnorm(x, mu, sigma)
pnCorr <- 1 - pnorm(x + 0.5, mu, sigma)

pb <- round(pb, 4)
pnUC <- round(pnUC, 4)
pnCorr <- round(pnCorr, 4)

df <- cbind(n = n, x = x, pb = pb,
            pnUC = pnUC, pnCorr = pnCorr)
View(df)


# Using R stats functions
# Simulate a pair of coins
d <- dbinom(0:2, 2, 0.5)
barplot(d, names.arg = 0:2)
