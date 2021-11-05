# Lecture 4


# Refugees analysis

ref <- c(123, 70, 93, 157)          # refugee data
aihw <- c(70.65, 18.5, 7.41, 3.43)  # AIHW data

cat <- c('low', 'moderate', 'high', 'very high')
names(ref) <- cat  # set names to the vector contents

# number of refugees; sample size
sampleSize <- sum(ref)

# Convert counts to percentages
refPercent <- (ref / sampleSize) * 100

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


# eels

eels <- matrix(c(264, 161, 127, 116, 99, 67), ncol = 3)

speciesLabels <- c('G.moringa', 'G.vicinus')
locationLabels <- c('Border', 'Grass', 'Sand')

dimnames(eels) <- list(species = speciesLabels,
                       location = locationLabels)
eels

sampleSize = sum(eels)

speciesCount = rowSums(eels)
locationProps = colSums(eels) / sampleSize

# our expected distribution
exp <- outer(speciesCount, locationProps)

# difference between eels and expected
cs <- sum((eels - exp)^2 / exp)


# Simulate / the distribution of differences
#
# simulate the data, assuming that the species
# does not effect the location
speciesProps <- speciesCount / sampleSize

d <- replicate(1000,
               {
                # sample of species
                sp <- sample(speciesLabels,
                             size = sampleSize,
                             replace = TRUE,
                             prob = speciesProps)
                
                # sample of locations
                lc <- sample(locationLabels,
                             size = sampleSize,
                             replace = TRUE,
                             prob = locationProps)
                
                # tabulate the results
                res <- table(sp, lc)
                
                
                # re-compute the expected
                r <- rowSums(res)
                c <- colSums(res) / sum(res)
                
                ex <- outer(r, c)
                
                
                # compute diff between sample and expected
                sum((res - ex)^2 / ex)
})

hist(d, col = 'skyblue', main = 'Distribution of differences')
abline(v = cs, col = 'purple', lwd = 3, lty = 3)

pVal <- mean(d > cs)  # p-value
pVal


# H0: Not difference in habitat preference
#     between the species

# H1: There is a difference in habitat
#     preference between the species

# CV = 0.05 (5%)
