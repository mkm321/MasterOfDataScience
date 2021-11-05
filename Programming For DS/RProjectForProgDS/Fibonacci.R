last1 <- 0
last2 <- 1
print(last1)
while(last1 < 100 && last2 < 100){
  print(last2)
  helper <- last2
  last2 = last1 + last2
  last1 = helper
}