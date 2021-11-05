#####################################
# Data Science Spring 2021          #
# Dr Franco Ubaudi                  #
#                                   #
# randomness of digits of pi        #
#####################################

trials <-  2500

df <- read.csv('pi500.csv')
head(df)

digitCount <- nrow(df) / length(obs)

digitsOfPi <- table(df$pi.digits)
exp <- rep(1, 10) * digitCount

digitsOfPi; exp

# Calculate difference between observed and expected
cs <- sum((digitsOfPi - exp)^2 / exp)
cs

# Simulate a random digits
d <- replicate(trials,
               {
                 obs <- sample(0:9, nrow(df), replace = TRUE)
                 obs <- table(obs)
                 
                 sum((obs - exp)^2 / exp)
               })

range(d)	# Limits of cs values resulting from trials

# Distribution of cs results for random digits
hist(d, col = 'skyblue', xlim = c(0, 40),
     main = '', xlab = 'distance')

# Location of cs & limits for random digits
abline(v = range(d), col = 2, lwd = 2, lty = 2)
abline(v = cs, col = 'purple', lwd = 3, lty = 3)
