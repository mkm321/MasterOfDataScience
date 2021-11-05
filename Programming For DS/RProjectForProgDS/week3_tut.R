## Checksum
v <- sample(0:9, size = 7, replace = TRUE)
checkSumVal <- 10- (sum(v) %% 10)
vcheck <- c(v,checkSumVal) ## valid scenario

e <- sample(c(1, rep(0, 7)))*sample(c(1,-1)) 
vcheckCorrupt <- vcheck + e ## invalid scenario

evaluatingVector <- vcheckCorrupt
if((sum(evaluatingVector) %% 10) == 0){
  print("Valid")
} else{
  print("Invalid")
}

## Buttonmen
p1d1 <- sample(1:10, size=1)
p1d2 <- sample(1:10, size=1)
p2d1 <- sample(1:10, size=1)

if(p1d1 >= p2d1 || p1d2 >= p2d1){
  print("Power Attack is possible.")
} else{
  print("Power Attack is not possible.")
}

if(p1d1 == p2d1 || p1d2 == p2d1 || (p1d1+p1d2) == p2d1){
  print("Skill Attack is possible.")
} else{
  print("Skill Attack is not possible.")
}

## Largest Number
n <- 10
v <- sample(100, size = n)
largestNumber <- -1
for(i in v){
  if(largestNumber < i){
    largestNumber <- i
  }
}
v
largestNumber

## Geometric Series
n <- 10
r <- 0.5
count <- 0
for(k in 0:n){
  count <- count + (r^k)
}
count

val <- (1 - r^(n+1)) / (1 - r)
val
if(count == val){
  print("equal")
} else {
  print("Not equal")
}

## Fibonacci Sequence

last1 <- 0
last2 <- 1
print(last1)
while(last1 < 100 && last2 < 100){
  print(last2)
  helper <- last2
  last2 = last1 + last2
  last1 = helper
}

## Prime Numbers

n <- 1000
v <- c()
for(i in 1:1000){
  count <- 0
  for(j in 1:i){
    if(i %% j == 0){
      count <- count + 1
    }
  }
  
  if(count == 2){
    print(i)
    v <- c(v,i)
  }
}

hist(v)
