# Slide 11
# F stat for 'hair1.csv'

df <- read.csv('hair1.csv')

ns <- aggregate(Pain ~ HairColour, df, length)
means <- aggregate(Pain ~ HairColour, df, mean)
vars <- aggregate(Pain ~ HairColour, df, var)

K <- length(unique(df$HairColour))
n <- nrow(df)

xBar <- mean(df$Pain)

SSB <- sum(ns$Pain * ((means$Pain - xBar)^2))
SSW <- sum((ns$Pain - 1) * vars$Pain)

fStat <- SSB / (K - 1)
t <- SSW / (n - K)
fStat <- fStat / t
fStat


# Slide 13
res <- oneway.test(Pain ~ HairColour, data = df, var.equal = TRUE)
res

fStat <- res$statistic

d <- replicate(1000,
               {
                 s <- sample(df$HairColour)
                 oneway.test(Pain ~ s, data = df,
                             var.equal = TRUE)$statistic
               })

mean(d > fStat)

hist(d)


# Slide 21
n <- 20  # sample size per group
K <- 10  # groups / categories

grp <- rep(1:K, each = n)
x <- rnorm(length(grp))  # generate sample data from SAME population

mns <- tapply(x, grp, mean)  # calculate group means

# calc pooled variance
sp <- sum((table(grp) - 1) * tapply(x, grp, var))
sp <- sp / (length(grp) - length(mns))

sp <- sqrt(sp)  # pooled standard deviation


# compute worst case t-statistic
max.tStat <- diff(range(mns)) / (sp * sqrt(2 / n))
max.tStat

pVal <- pt(-max.tStat, 2*n - 2) * 2
pVal

pVal < 0.05


# Slide 25
df <- read.csv('hair1.csv')

fit <- aov(Pain ~ HairColour, df)
fit

summary(fit)

TukeyHSD(fit)


# Slide 28
par(mar = c(3, 11, 3.5, 0.5), cex = 0.7)
plot(TukeyHSD(fit), las = 1)
