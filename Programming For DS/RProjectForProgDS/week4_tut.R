v <- c("dog", "cat", "chicken", "rabbit", "cow", "horse")
m <- matrix(1:20, nrow = 4, ncol = 5)
l <- list(list(method = "knn", repetitions = 40, accuracy = 0.67, std_err = 0.04),
          list(method = "svm", repetitions = 20, accuracy = 0.78, std_err = 0.12)) 
d <- data.frame(country = c("Australia", "New Zealand", "India"),
                battingAverage = c(25.6, 24.8, 29.3), 
                bowlingAverage = c(22.1, 21.6, 25.2))
head(d)
# 1.
v[3]
# 2
for(i in 1:length(v)){
  if(i != 3){
    print(v[i])
  }
}
v[-3]
# 3
v[2]; v[4]
v[c(2,4)]
# 4
v[-c(2,4)]
# 5
m[2,3]
# 6
m[,5]
# 7
m[2,]
# 8
m[1:2, 3:4]
# 9
m[, -3]
# 10
m[-1,]
# 11
l[[1]]
# 12
l[[2]]["accuracy"]

l[[2]]$accuracy
# 13
l[[1]][c("method", "accuracy")]
# 14
d[,1]
d$country
# 15
d[2,]
# 16
d[, "battingAverage"]
# 17
d[, c("country", "battingAverage")]
# 18
d[1, "battingAverage"]
# 19
d[1:2, "battingAverage"]
# 20
d[1:2, c("country", "battingAverage")]


## Question 2

# 1
v[3] <- "goat"
# 2
v[4:5] <- "goat"
# 3
v[c(1,5)] <- c("rat","snake")
# 4
m[3,4] <- 100
# 5
m[,2] <- 100
# 6
m[3,] <- 90
# 7
m[4,] <- 1:5
# 8 Insert the values at rows one and two and columns one and two
# (a 2 × 2 matrix) into the positions at rows three and four and
# columns four and five.

m[3:4, 4:5] <- m[1:2,1:2]
# 9
l[[1]]$method <- "xnn"
# 10
l[[1]]$validated <- TRUE
# 11
l[[2]] <- "NA"
# 12
d[3, "battingAverage"] <- 31.2
# 13
d[, "bowlingAverage"] <- NA
# 14
d[, "country"] <- c("England", "Zimbawe", "South Africa")

## Question 3

mat <- matrix(0, nrow = 100, ncol = 100)
for (i in 1:100) {
  for(j in 1:100) {
    mat[i,j] <- i+j 
  }
}
image(mat)
for (i in 1:100) {
  for(j in 1:100) {
    mat[i,j] <- (i^2)+(j^2) 
  }
}
image(mat)

## Question 4

table <- data.frame(ID = c(580,625,630,721,700,855,740,187,571,312),
                    Subject = c("English", "English", "Science", "Science", "Maths", "English", "Science", "Science",
                                "English", "Maths"),
                    Mark = c(85,69,52,76,63,58,74,66,95,77))
# computing the average mark for each of the subject
eng <- table[table$Subject == "English" | table$Subject == "Science",]
eng1 <- table[which(table[,"Subject"] == c("English","Science")),]
sci <- table[which(table[,"Subject"] == "Science"),]
maths <- table[which(table[,"Subject"] == "Maths"),]
mean(eng[,3])
mean(sci[,3])
mean(maths[,3])

## Question 5

## Question 6
# get our bonus
bonus <- 2 ## calculated
## roll a die
roll <- sample(1:20, 1)
# see if we pass the check condition of > 10
checkWisdom <- function(data, attr = "wis", t = 10)
{  
  a <- data
  roll <- sample(1:20, 1)
  wisdom_bonus <- floor(a$attributes[[attr]] - 9 / 2)
  if(roll + wisdom_bonus > t){
    return(TRUE)
  } else return(FALSE)
}

## Question 7 Myster number
n <- 100
mysteryNumber <- sample(n, size = 1)
candidates <- 1:100
while(length(candidates) > 1){
  middle <- round(length(candidates) / 2)
  if(mysteryNumber > candidates[middle]) {
    candidates <- candidates[middle+1]: candidates[length(candidates)]
  } else {
    candidates <- candidates[1]: candidates[middle]
  }
}

print("The mystery number is: ")
print(candidates)
## Question 8 Logistic Map
n <- 1000
r <- 3.75
x <- rep(0,n)
x[1] <- 0.5
for(i in 2:n){
  x[i] <- r*x[i-1]*(1 - x[i-1])
  
}
plot(x, type="l")
