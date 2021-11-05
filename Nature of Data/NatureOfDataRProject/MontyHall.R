runs <- 1000
n = 0

for(i in 1:runs) {
  door = c(1,2,3)
  choice = sample(door,1)
  cardoor = sample(door,1)
  if (choice == cardoor) {
    n = n + 1
  }
}

n/runs

n = 0
door <- c(1,2,3)
for ( i in 1:runs) {
  cardoor = sample(door,1)
  select = sample(door,1)
  remove = ifelse(cardoor==select, 
                  sample(setdiff(door,cardoor),1),
                  setdiff(door,c(cardoor,select)))
  reselect <- setdiff(door,c(select,remove))
  if (cardoor == reselect) {
    n = n + 1
  }
}
n/runs