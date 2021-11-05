# Your R code to answer the question
# Loading the above table content into an appropriate data structure
fatalities <- c(31,19,21,7,14,3,4,1)
population <- c(32,26,20,7,10,2,1,2)

states = c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT")
names(fatalities) = states
names(population) = states

data <- rbind(fatalities,population)
data
total_fatalaties <- 1235
obs <- (fatalities * total_fatalaties)/100
obs
exp <- total_fatalaties * population / 100

cs <- sum((obs - exp) ^2 / exp)
cs

d <- replicate(1000, {
  res <- rmultinom(1,
                   total_fatalaties,
                   exp)
  
  sum((res - exp) ^2 / exp)
})
hist(d)
hist(d, col = 'skyblue', main = 'Distribution of differences',
     xlim = c(0, 170))
abline(v = cs, col = 'purple', lwd = 3, lty = 3)

pVal <- mean(d > cs)  # p-value
pVal

chisq.test(obs, p=exp, rescale.p=TRUE, simulate.p.value = TRUE, B=1000)
