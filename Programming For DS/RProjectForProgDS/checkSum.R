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