# creating a vector of coin.
# this is the hypothesis testing.
coin <- c('H', 'T')
# setting variables number of flips and trials
flips <- 100
trials <- 5000

# let's say we got this number after 100 flips
observations <- c(70,30)
# expected results
exp <- c(0.5,0.5) * flips
# figuring out difference between what we observed and what 
# is expected
diff = observations - exp
# we want a single point of a number
sum((diff)^2)
# formula for getting single number
# major of difference between what we observed and what we expect
cs <- sum((observations - exp)^2/ exp)

# now we gonna simulate what actual coin is gonna do
# for this we will use sample
result <- sample(coin, flips, replace = TRUE)
# now we use table functions
res <- table(result)
# now again we gonna find the difference
diff <- sum((res - exp)^2 / exp) # now that will be less

obsList <- replicate(trials, {
  result <- sample(coin, flips, replace = TRUE)
  # now we use table functions
  res <- table(result)
  # now again we gonna find the difference
  sum((res - exp)^2 / exp) # now that will be less
})
# range will tell us the lower and upper bound
range(obsList)
# showing the results
hist(obsList, xlim = c(0,20))
abline(v = range(obsList), col=2, lwd=2, lty=2)
abline(v = cs, col= "purple", lwd=3, lty=3)

# a range of results we get from flipping a fair coin, now it looks like purple is not a fair coin.
