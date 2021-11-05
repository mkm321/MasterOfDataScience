eels <- matrix(c(264, 161, 127, 116, 99, 67), ncol = 3) # EELS Dataset

speciesLabels <- c('G.moringa', 'G.vicinus') ## lables of species
locationLabels <- c('Border', 'Grass', 'Sand') ## lables of locations

dimnames(eels) <- list(species = speciesLabels,
                       location = locationLabels)
eels

sampleSize = sum(eels) ## Total number of eels

speciesCount = rowSums(eels) ## Total counts as per the species
locationProps = colSums(eels) / sampleSize ## Probabilites of the locations

# our expected distribution
exp <- outer(speciesCount, locationProps) ## expected number of values for eels

# difference between eels and expected
cs <- sum((eels - exp)^2 / exp) ## chi squared distance - 6.26


# Simulate / the distribution of differences
#
# simulate the data, assuming that the species
# does not effect the location
speciesProps <- speciesCount / sampleSize # Probability/distribution of species

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
                 # here we recomputed because earlier we calculated the expected value before as per it's
                 # distributions and not as per original counts.
                 # so, we need to recompute it everytime.
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
cv <- 0.05
cv > pVal ## can we reject the null hypothesis?

## now it came false, this means we cannot reject the null hypothesis.
## i.e they are same, or there is no difference in their habitat


# H0: Not difference in habitat preference
#     between the species

# H1: There is a difference in habitat
#     preference between the species

# CV = 0.05 (5%)