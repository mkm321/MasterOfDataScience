z <- 0
x <- 1
if(x>0){
  z <- x
}
z

z <- c()
for(i in -5:5){
  if(i > 0) {
    z <- c(z,i)
  }
  else {
    z <- c(z,0)
  }
}
z