# TODO 1: Write in the R to read in the data.
data <- read.csv("datasets/pi500.csv")
head(data)
dim(data)
# to see the status of the data we can use the summary
# command
summary(data)
# TODO 2: Now we'd like to use R to generate random digits.
digits <- sample(0:9, size=500, replace = TRUE)
x = table(digits)
# TODO 3: Now use the sum command in R to subtract.
sum((x-50)^2)
x <- table(data$pi.digits)
pi500 <- sum((x-50)^2)

digits500 <- replicate(1000, {
  digits <- sample(0:9, size=500, replace = TRUE)
  x = table(digits)
  sum((x-50)^2)
})

hist(digits500)
abline(v=range(digits500), col="red")
abline(v=pi500, col="purple")
