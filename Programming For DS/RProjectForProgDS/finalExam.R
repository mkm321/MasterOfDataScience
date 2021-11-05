### 1.1

colors <- c("green", "orange", "red")
print(paste("second element of the vector is: ",colors[2]))

### 1.2

for(color in colors){
  print(paste("I see a",color,"car."))
}

### 1.3

if(colors[3] == "yellow"){
  print(paste("Spotto!"))
}


### 3.1

addOne <- function(d){
  return(d+1)
}

addOne(2)

### 3.2

doOperations <- function(d){
  operations <- c()
  operations[1] <- sin(d)
  operations[2] <- d^5
  operations[3] <- cos(d)
  operations[4] <- d^4
  
  return(operations)
}

doOperations(2)


### 3.3

getResult <- function(d){
  result <- c()
  result[1] <- d[4]^4
  result[2] <- d[3]^2
  result[3] <- d[2]^3
  result[4] <- tan(d[3])
  
  return(result)
}

val <- c(1,2,3,4)
getResult(val)


### 5.1

student_honours_status <- read.csv("exam3table.csv")
names(student_honours_status)

### 5.2

library("tidyverse")
student_honours_status %>% filter(Honours == "Yes")

### 5.3

student_honours_status %>% filter(Degree == "BCom" & Grade == "P")


### 7.1

# rw------x 
# 110 000 001 6 0 1
# chmod 601 file.txt
