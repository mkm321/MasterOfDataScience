## QUESTION 1 -
## we want to understand if the digits of pi are random or form a pattern on the basis of number of digits.
## In this we will take first 50 pi digits and then see if the occurrence of those digits are same or are random
## Here we did hypothesis testing to determine whether the digits of pi are random.
#############################


## 1. Write in the R code cell below this one to read in the data, and run the code (click run above when done)
pi50 <- read.csv("datasets/pi50.csv")
head(pi50)
## To see the status of the data we can use the summary command:
summary(pi50)
## 2. Now we'd like to use R to generate random digits. This time we'll give you the code. Run it and observe
digits <- sample(0:9, size = 50 ,replace = TRUE)
pi50_random_table <- table(digits)
pi50_random_table
## 3. Now use the sum command in R to subtract (see above)
cs_random <- sum((pi50_random_table - 5)^2) ## we get 5 by - size = 50 divided by number of digits = 10 (0-9).
cs_random
# compare with the sum of squared differences for the digit of pi
pi50_table <- table(pi50$pi.digits)
pi50_table
cs <- sum((pi50_table - 5)^2)
cs
## now replicate the random output 1000 times
d <- replicate(1000, {
  digits <- sample(0:9, size = 50 ,replace = TRUE)
  pi50_random_table <- table(digits)
  sum((pi50_random_table - 5)^2)
})
hist(d)
abline(v = cs, col="red")

## 4. Repeat above using 100 digits (file `pi100.csv`) and 500 digits (`pi500.csv`). 
## Remember that the expected value of the random digits has changed

## pi100
pi100 <- read.csv("datasets/pi100.csv")
pi100_table <- table(pi100$pi.digits)
cs_100 <- sum((pi100_table - 10)^2)

d100 <- replicate(1000, {
  digits <- sample(0:9, size = 100, replace = TRUE)
  tab <- table(digits)
  sum((tab - 10)^2)
})
hist(d100)
abline(v = cs_100, col = "red")

## pi500
pi500 <- read.csv("datasets/pi500.csv")
pi500_table <- table(pi500$pi.digits)
cs_500 <- sum((pi500_table - 50)^2)

d500 <- replicate(1000, {
  digits <- sample(0:9, size = 500, replace = TRUE)
  tab <- table(digits)
  sum((tab - 50)^2)
})
hist(d500)
abline(v = cs_500, col = "red")

