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