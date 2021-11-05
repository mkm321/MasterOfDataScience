iraqi = c(123, 70, 93, 157)
aihw = c(0.706533566731324, 0.185013246912996, 0.074141316175427, 0.0343118701802526)
levs = c("low", "moderate", "high", "very high")
names(iraqi) = levs
names(aihw) = levs
iraqi

# number of refugees; sample size
sampleSize <- sum(iraqi)
stess_simulation <- replicate(1000, {
  sample_stress <- sample(1:100, sampleSize, replace = TRUE)
  low_count = moderate_count = high_count = vhigh_count = 0
  for(i in 1:sampleSize) {
    if(sample_stress[i] <= 71){
      low_count <- low_count + 1
    } else if(sample_stress[i] >= 72 && sample_stress[i] <= 90){
      moderate_count <- moderate_count + 1
    } else if(sample_stress[i] >= 91 && sample_stress[i] <= 97){
      high_count <- high_count + 1
    } else {
      vhigh_count <- vhigh_count + 1
    }
  }
  
  res <- c(low_count, moderate_count, high_count, vhigh_count)
  sum((res - expected_value)^2 / expected_value)
})

t(stess_simulation)
observed_value <- iraqi
expected_value <- aihw * 443

cs <- sum((observed_value - expected_value)^2 / expected_value)

# hist(stess_simulation, col = 'skyblue', main = 'Distribution of differences')

hist(stess_simulation, col = 'skyblue', main = 'Distribution of differences',
     xlim = c(0, 1600))
abline(v = cs, col = 'purple', lwd = 3, lty = 3)


# H0: Iraqi refugee population stress same to Australian reference population.
# We try to see if the evidence is strong enough to reject H0.

pVal <- mean(stess_simulation > cs)  # p-value
pVal
