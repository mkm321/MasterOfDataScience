die <- c('Y', 'N')
rolls <- 6
trials <- 10000

# Approach I
# missing : 0.25 - define, 
d <- replicate(trials,
               {
                 res <- sample(die, size = rolls,
                               replace = TRUE, prob = c(0.25,0.75))
                 (length(res[res == 'Y']) >= 5)
               })

res <- table(d)
res[2] / trials
# another one,
d<- replicate(10000, {
  res<-rbinom(1:6,6,prob = 0.25)
  
})
set.seed(2)
d <- rbinom(10000,6, prob = 0.25)
d
result <-table(d)
l<-length(result)
res <- (result[l]+result[l-1])/sum(result)
res[[1]]
class(res)
res[1]
names(res) <- ""
res
pbinom(4, 6, 0.25, lower.tail=FALSE)
