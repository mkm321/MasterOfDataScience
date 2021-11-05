# How big do we want the group to be
n <- 23

# Use a loop to create a vector of probabilities
probs <- vector(length = n)
for (i in 1:n) {
  # The probability of a new group member having a different birthday
  probs[i] <- (365-i)/365 
  
  # The number of people in the group
  names(probs)[i] = i+2
}
print(probs)