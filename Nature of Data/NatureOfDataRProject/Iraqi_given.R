# Refugees analysis

ref <- c(123, 70, 93, 157)          # refugee data in counts
aihw <- c(70.65, 18.5, 7.41, 3.43)  # AIHW data in proportions

cat <- c('low', 'moderate', 'high', 'very high')
names(ref) <- cat  # set names to the vector contents

# number of refugees; sample size
sampleSize <- sum(ref)

# Convert counts to percentages
# refPercent <- (ref / sampleSize) * 100

# Create a matrix / table containing the refugees
# and AIHW data
m <- rbind(refPercent, aihw)
barplot(m, beside = TRUE)

barplot(m, beside = TRUE, col = 2:3,
        legend.text = c('Refugees', 'Australians'))

# observation to be evaluated
obs <- ref

# given the sample size, create a
# distribution consistent with AIHW
exp <- sampleSize * aihw / 100

# compute different between refugees and expected
cs <- sum((obs - exp)^2 / exp)

d <- replicate(1000,
               {
                 # create a sample
                 # simulate a sample from the australian
                 # population of size 443; hence total refugees
                 res <- rmultinom(1,           # create single sample
                                  sampleSize,  # sample size
                                  exp)         # distribution of sample
                 
                 # compute diff between sample and AIHW
                 sum((res - exp)^2 / exp)
               })

hist(d, col = 'skyblue', main = 'Distribution of differences')

hist(d, col = 'skyblue', main = 'Distribution of differences',
     xlim = c(0, 1600))
abline(v = cs, col = 'purple', lwd = 3, lty = 3)

pVal <- mean(d > cs)  # p-value
pVal