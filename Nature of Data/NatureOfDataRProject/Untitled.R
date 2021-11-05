questionOutcomes <- c('C', 'NC')
flips <- 6			  
trials <- 500			# Number of trials or experiments to perform

# Secret code to create our unknown coin results
if(TRUE)
{
  quesResults <- sample(questionOutcomes, flips, replace = TRUE, prob = c(0.25, 0.75))
  quesResults <- table(quesResults) 
}

quesResults
exp <- c(0.25, 0.75) * flip

quesResults; exp			# View variable contents

cs <- sum((quesResults - exp)^2 / exp)
cs

# Simulate a fair coin 
d <- replicate(trials,
               {
                 obs <- sample(coin, flips, replace = TRUE)
                 obs <- table(obs)
                 
                 sum((obs - exp)^2 / exp)
               })

range(d)	# Limits of cs values resulting from trials
d
# Distribution of cs results for a fair coin
hist(d, col = 'skyblue')

# Location of cs; unknown coin results
abline(v = range(d), col = 2, lwd = 2, lty = 2)
abline(v = cs, col = 'purple', lwd = 3, lty = 3)
