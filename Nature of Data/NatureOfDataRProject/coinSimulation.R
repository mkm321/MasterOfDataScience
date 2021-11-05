#####################################
# Data Science Spring 2021          #
# Dr Franco Ubaudi                  #
#                                   #
# foundations of hypothesis testing #
#####################################

coin <- c('H', 'T') # coin definition
flips <- 100			  # Number of coin flips
trials <- 5000			# Number of trials or experiments to perform

# Secret code to create our unknown coin results
if(TRUE)
{
  coinResults <- sample(coin, flips, replace = TRUE,
                        prob = c(0.65, 0.35))
  coinResults <- table(coinResults)
#coinResults <- c(70, 30)  
}

coinResults					# Unknown coin results
exp <- c(0.5, 0.5) * flips	# What is expected from a fair coin

coinResults; exp			# View variable contents

# Calculate difference between unknown and expected coins
cs <- sum((coinResults - exp)^2 / exp)
cs

# Simulate a fair coin 
d <- replicate(trials,
               {
                 obs <- sample(coin, flips, replace = TRUE)
                 obs <- table(obs)
                 
                 sum((obs - exp)^2 / exp)
               })

range(d)	# Limits of cs values resulting from trials

# Distribution of cs results for a fair coin
hist(d, col = 'skyblue', xlim = c(0, 20))

# Location of cs; unknown coin results
abline(v = range(d), col = 2, lwd = 2, lty = 2)
abline(v = cs, col = 'purple', lwd = 3, lty = 3)
