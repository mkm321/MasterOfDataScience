# Same as for week 04


###########################################################


# plus the following

# looking at maternal smoking data
df <- read.csv('birthwt.csv')

d <- aggregate(df$bwt, list(df$smoke), length)
barplot(d$x, names.arg = d$Group.1)


aggregate(df$bwt, list(df$smoke), mean)

# aggregate(data to be chopped up,
#			how to chop the data up -> can be multiple ways so all ways provided in a list,
#			what to do with the pieces obtained via chopping)


###########################################################


# two ways to calculate a binomial probability
p <- 0.5	# probability of success
n <- 10		# number of trials
k <- 4		# number of successes

# exactly via theory
dbinom(k, n, p)
# [1] 0.2050781


# the hard theory way, but still by theory
# P(X = x) = p^n    where x = for each event
eachEventProbability <- p^n

# possible combinations, or number of events with same k value
combs <- choose(n, k)

probEstimate <- eachEventProbability * combs
probEstimate  # of cause same as using dbinom
# [1] 0.2050781   as expected the same answer


# by estimation using simulation
d <- replicate(100000,
               {
                 s <- sample(0:1, 10, replace = TRUE)
                 
                 # count number of heads & check IF equal to k
                 sum(s) == k
               })

mean(d)		# estimated probability
# [1] 0.20632


###########################################################


# using a normal distribution, your Ps & Qs
# pnorm gives probability given a q value (x axis value)
# qnorm gives q value given a probaility

# assume a stndard normal; mean of zero, std dev of one
# P(X < 1) = pnorm(1)
# [1] 0.8413447

# what x value gives a probability of 0.8413447 in the left tail
qnorm(0.8413447)
# [1] 0.9999998 some rounding issues, but same answer


# Same thing applies with discrete, but can be tricky due to rounding
p <- pbinom(12, 300, 0.5)
p
# [1] 4.546508e-70

qbinom(p, 300, 0.5)
[1] 12