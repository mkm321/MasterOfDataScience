cv <- 0.05

## as east outperforms west, we need to find new outperforms ref.
## i.e mu(new) - mu(ref) > 0 or mu(new) > mu(ref)
drugs <- read.csv("datasets/drugs.csv")
head(drugs)

delta <- aggregate(response ~ drug, drugs, mean)$response

cs <- -diff(delta)

d <- replicate(5000, {
  shuffled_drug <- sample(drugs$drug)
  delta <- aggregate(response ~ shuffled_drug, drugs, mean)$response
  c <- -diff(delta)
})

mean(d)
range(d)

hist(d, col = "skyblue")
abline(v = cs, col = "red", lwd = 2)

count <- sum(d > cs)
pvalue <- count/length(d)
pvalue

cv > pvalue
## if true then reject the null hypothesis.