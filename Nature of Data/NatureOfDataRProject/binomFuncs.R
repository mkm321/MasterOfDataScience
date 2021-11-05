# Simulate flipping a coin ten times,
# where probability of success is 0.7
outcomes <- 0:10
p <- 0.7

# Individual outcome probabilities
d <- dbinom(outcomes, 10, p)
res <- barplot(d, names.arg = outcomes, ylim = c(0, 0.35))
text(res, d, round(d, 2), pos = 3, col = 'red')

# Cumulative probabilities
prob <- pbinom(outcomes, 10, p)
text(res, d + 0.02, round(prob, 2), pos = 3, col = 'green')

dbinom(2,3,0.5)
choose(20,15)
