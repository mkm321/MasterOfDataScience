## DATA
## 1 Extracting Rows

# Perform the following actions using the mtcars data found in R. Read
# the mtcars help file to see an explanation of the variables.
data(mtcars)
head(mtcars)
# • Create a data frame containing all cars that have a six cylinder engine.
mtcars_with_6_cyl <- subset(mtcars, subset = c(cyl == 6))

# • Create a data frame containing all cars that have automatic trans- mission.
mtcars_with_am <- mtcars[mtcars$am == 1,]

# • Create a data frame containing all cars that have a horsepower less than 100.
mtcars_horsepower <- mtcars[mtcars$hp < 100, ]

# • Create a data frame containing all cars that have a rear axle ratio between 3 and 4.
# • Create a data frame containing all cars that have a V shaped en- gine and less than six cylinders.
# • Create a data frame containing all cars that have an automatic transmission 
#   and more than three forward gears.
# • Create a data frame containing all cars that have a horsepower less than 100 or greater than 200.
# • Create a data frame containing all cars that provide at least 20 miles per 
#   gallon with six cylinders, or at least 30 miles per gallon with four cylinders.
mtcars_mpg_cyl <- mtcars[(mtcars$mpg >= 20 && mtcars$cyl == 6) || (mtcars$mpg >= 30 && mtcars$cyl == 4), ]

mtcars_updated <- subset(mtcars, subset = ((mpg >= 20 & cyl == 6) | (mpg >= 30 & cyl == 4)))
# • Create a data frame containing all cars whose horsepower divided by the number 
#   of cylinders is greater than 30.
subset(mtcars, subset = ((hp/cyl) > 30))


## 2 Locating Rows
# Perform the following actions using the mtcars data found in R. Read
# the mtcars help file to see an explanation of the variables.
#  • Which car has the highest miles per gallon?
pos <- which.max(mtcars$mpg)
mtcars[pos,]

max(mtcars$mpg)
#  • Which car has the greatest value for horsepower divided by cylinders?

max(mtcars$hp / mtcars$cyl)
mtcars[which.max(mtcars$hp / mtcars$cyl),]
#  • Which six cylinder car has the highest miles per gallon?
mtcars_cyl <- subset(mtcars, subset = c(cyl == 6))
mtcars_cyl[which.max(mtcars_cyl$mpg), ]

#  • What is the standard deviation of the displacement for cars that
# have four cylinders?
mtcars_cyl <- subset(mtcars, subset = c(cyl == 4))
sd(mtcars_cyl$disp)
#  • What proportion of V-shaped engine cars are automatic?

mean(subset(mtcars, vs==0)$am == 0)
#  • Tabulate the number of carburettors for the set of cars that provide more than 25 miles per gallon.
mtcars_mpg <- subset(mtcars, subset = c(mpg > 25))
table(mtcars_mpg$carb)

# • Plot a histogram of the rear axle ratio.

hist(mtcars$drat)
# • Provide a two way table containing the count of the combinations
# of engine type and transmission type.
# engine is vs and transmision is am
table(Engine = mtcars$vs, Transmission = mtcars$am)


## Cornavirus

