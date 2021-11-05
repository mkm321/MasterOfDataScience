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