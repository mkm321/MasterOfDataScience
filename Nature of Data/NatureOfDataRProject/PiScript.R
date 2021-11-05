# Are the digits of Pi Random?
trials <- 2500
pi500 <- read.csv("datasets/pi500.csv")
head(pi500)
attach(pi500)

digitCount <- nrow(pi500) / 10

obs <-  table(pi.digits)
exp <- rep(digitCount, 10)

cs <- sum((obs - exp)^2 / exp)

randomDigits <- replicate(trials, {
  sampleData <- sample(0:9, nrow(pi500), replace = TRUE)
  sampleData <- table(sampleData)
  
  sum((sampleData - exp)^2 / exp)
})

hist(randomDigits, xlim = c(0,40))
abline(v = range(randomDigits), col = 2)
abline(v = cs, col = "purple")
# this represents that it is inside this distribution
# of number that digits of pi really do look random.
