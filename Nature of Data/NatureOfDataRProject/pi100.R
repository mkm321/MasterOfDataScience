# TODO 1: Write in the R to read in the data.
data <- read.csv("datasets/pi100.csv")
head(data)
dim(data)
# to see the status of the data we can use the summary
# command
summary(data)
# TODO 2: Now we'd like to use R to generate random digits.
digits <- sample(0:9, size=100, replace = TRUE)
x = table(digits)
# TODO 3: Now use the sum command in R to subtract.
sum((x-10)^2)
x <- table(data$pi.digits)
pi100 <- sum((x-10)^2)

digits100 <- replicate(1000, {
  digits <- sample(0:9, size=100, replace = TRUE)
  x = table(digits)
  sum((x-10)^2)
})

hist(digits100)
abline(v=range(digits100), col="red")
abline(v=pi100, col="purple")
