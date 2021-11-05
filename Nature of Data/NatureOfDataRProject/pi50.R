# TODO 1: Write in the R to read in the data.
data <- read.csv("datasets/pi50.csv")
head(data)
dim(data)
# to see the status of the data we can use the summary
# command
summary(data)
# TODO 2: Now we'd like to use R to generate random digits.
digits <- sample(0:9, size=50, replace = TRUE)
x = table(digits)
# TODO 3: Now use the sum command in R to subtract.
sum((x-5)^2)
x <- table(data$pi.digits)
pi50 <- sum((x-5)^2)

digits50 <- replicate(1000, {
  digits <- sample(0:9, size=50, replace = TRUE)
  x = table(digits)
  sum((x-5)^2)
})

hist(digits50)
abline(v=range(digits50), col="red")
abline(v=pi50, col="purple")
