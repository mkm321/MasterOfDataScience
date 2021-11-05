die <- 1:6
rolls <- 5
trials <- 100000

# Approach I
d <- replicate(trials,
              {
                res <- sample(die, size = rolls,
                              replace = TRUE)
                length(unique(res)) != rolls
              })

res <- table(d)
res[2] / trials
