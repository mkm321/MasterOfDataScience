x <- sample(c("H", "T"), replace = TRUE, size = 20)
sum(x == "H")

n = 8
p = 0.1
# mu
n*p
# variance
n*p*(1-p)

b <- rbinom(100,8,0.1)
mean(b)
var(b)
?ppois

pbinom(3, 20, 0.05, lower.tail = FALSE)
