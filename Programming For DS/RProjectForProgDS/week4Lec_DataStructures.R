## Data Structures.

# vectors

heights <- c(173.2,170,180,176.3)
class(heights)
heights[1]
heights[-1]
length(heights)
heights[c(2,3)]
heights[c(2,3)] <- c(175.9, 178.6)
heights[heights > 170 && heights < 175]


###########################
##  BMI calculator
w <- c(92.5, 67.8, 72.1, 79.1)
h <- c(1.64, 1.72, 1.77, 1.81)
bmi <- rep(0, times=length(w))

for(i in 1:length(w)){
  bmi[i] <- w[i] / h[i]^2
}
index <- which(bmi > 25)
bmi[index]

#########################

ages <- c("Garrey" = 12, "Larry" = 25)

ages[1]
ages["Garrey"]
names(ages) <- c("Mike", "Phoebe")


###############################

## Matrices

B <- matrix(1:12, nrow = 3, ncol = 4)
B[1:2, 3:4]
X <- B[1,]
X
X <- B[1,,drop=FALSE]
X

A <- matrix(1:6, nrow = 2, ncol=3)
A[c(2), c(1,3)] <- c(7,8)

##########################
x <- c(3,4,6)
y <- c(2,-1,0)
cosineAn <- (x %*% y) / (sqrt(x %*% x) * sqrt(y%*%y))
#########################

w <- c(92.5, 67.8, 72.1, 79.1)
h <- c(1.64, 1.72, 1.77, 1.81)
bmi <- rep(0, times=length(w))

bmi <- w / (h^2)

######################

servers <- list(list(Server_Name = "frodo", OS = "Linux", Hack_Attempts = c(2,5,10,15)), 
                list(Server_Name = "bilbo", OS = "Windows", Hack_Attempts = c(13,3,14,10))) 

servers$bilbo$Hack_Attempts

######################

aggregate(mpg ~ cyl, mean, data=mtcars)
with(mtcars, table(cyl, gear))              
