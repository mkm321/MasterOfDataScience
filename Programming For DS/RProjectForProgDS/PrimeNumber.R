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
x = 5
checkPrime <- function(x) {
  if(x == 1 && x < 1){
    return("Hi")
  }
  for(i in 2:(x-1)){
    if(x %% i == 0) {
      return(FALSE)
    }
  }
  return(TRUE)
}
