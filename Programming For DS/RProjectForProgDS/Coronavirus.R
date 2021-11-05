# Each row of the data shows the time and the expected number
# of people in each state, so the sum of expected values in each state 
# should be the population size of Sydney.
c19 <- read.csv("datasets/epiSEIHCRD_combAge.csv")
head(c19)

pop <- apply(c19[, -1], 1 , sum)
pop[1]
which(pop != 5500000)

rowSums(c19[, -1])

## Examining Data

str(c19)
## looks like data matches

# Check that the sum of expected values in each state for each time sums to the population of Sydney.

## Visualise

plot(rowSums(c19[, -1]), ylim = c(0, 6000000))

plot(c19$t, c19$S, type="l", ylim = c(1000, 5500000), log = "y", col = 1, xlab = "Total number of days",
     ylab = "Population")
lines(c19$t, c19$E, col = 2)
lines(c19$t, c19$I, col = 3)
lines(c19$t, c19$H, col = 4)
lines(c19$t, c19$C, col = 5)
lines(c19$t, c19$R, col = 6)
lines(c19$t, c19$D, col = 7)
legend("bottomright",legend = colnames(c19)[-1], col = 1:7, lty = 1)


## Analysing Data
# Use the data to answer the following questions. Write code to find the solutions.
#  • When will more than 1000 hospital beds be required?

c19[c19$H > 1000,][1,1]


#  • How long is the period for which more than 1000 hospital beds are
#    required?

max(c19[c19$H > 1000,][,1]) - c19[c19$H > 1000,][1,1]

#  • When will the number of deaths reach more than 100 expected per
#    day?

# c19[c19$D > 100,][1,1]

deathsd1 <- c19$D[-c(1:4)]
deathsd0 <- c19$D[1:(length(c19$D)-4)]

time <- c19$t[1:length(deathsd0)]
head(time)
dailyDeaths <- deathsd1 - deathsd0
plot(time, dailyDeaths, type="l")

pos <- min(which(dailyDeaths > 100))
time[pos]

#  • When will the most number of people be infected?
pos <- which.max(c19$I)
c19[pos,][1,1]
#  • When will the hospitalisation rate start to decrease?