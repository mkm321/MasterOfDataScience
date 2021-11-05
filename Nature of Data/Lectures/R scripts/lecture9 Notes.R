# Slide 4
df <- read.csv('crabsmolt.csv')

plot(df$postsz ~ df$presz, pch = 19, col = 1, 
     xlab = 'presz', ylab = 'postsz')
# Add a line with zero intercept and slope one
abline(coef = c(0, 1), lwd = 3)

# Easy way to create linear regression model
m <- lm(postsz ~ presz, df)
m

# Add the actual line contained in m
abline(m, col = 2, lwd = 2, lty = 2)


# For slide 14
n <- nrow(df)  # Get number of rows
means <- apply(df, 2, mean)  # 2 means traverse each column
n; means  # means contains xBar and yBar

# Refer slide 13 for COV(x,y), which is ssxy
SSxx <- sum((df$presz - means[1])^2)  # sum((x - xBar)^2)
SSyy <- sum((df$postsz - means[2])^2) # sum((y - yBar)^2)
SSxy <- sum((df$presz - means[1]) *
              (df$postsz - means[2]))   # sum((x - xBar)(y - yBar))
SSxx; SSyy; SSxy

bHat <- SSxy / SSxx  # Slope of the line, ref slide 13
aHat <- means[2] - bHat * means[1]  # line intercept, ref slide 13
bHat; aHat

# Easy way to create linear regression model
m <- lm(postsz ~ presz, df)
m


# Slide 20
# Hypothesis test earnings / height
df <- read.csv('heights.csv')

# H0: b = 0
# HA: b <> 0
#
# CV = 0.05

m <- lm(earn ~ height, df)
b <- coef(m)[2]

d <- replicate(2000,
               {
                 s <- sample(df$height)
                 n <- lm(earn ~ s, df)
                 coef(n)[2]
               })

hist(d, xlim = c(-1, 1) * 1600)
abline(v = c(-1, 1) * b, col = 2, lwd = 2)

pVal = mean(abs(d) > abs(b))
pVal

# Since CV > pVal, reject H0
# i.e. population slope is NOT 0
# i.e. there is at least some connection between height and earning


# no slide; could population slope be zero?
df <- read.csv('crabsmolt.csv')

# H0: b = 0
# HA: b <> 0
#
# CV = 0.05

m <- lm(postsz ~ presz, df)
b <- coef(m)[2]

d <- replicate(2000,
               {
                 s <- sample(df$presz)
                 n <- lm(postsz ~ s, df)
                 coef(n)[2]
               })

hist(d, xlim = c(-1, 1))
abline(v = c(-1, 1) * b, col = 2, lwd = 2)

pVal = mean(abs(d) > abs(b))
pVal

# Since CV > pVal, reject H0
# i.e. population slope is NOT 0
# i.e. there is at least some connection between presz and postsz


# Slide 23
df <- read.csv('crabsmolt.csv')

# H0: b - 1 = 0    hence b == 1
# HA: b - 1 <> 0   hence b <> 1
#
# CV = 0.05

m <- lm(postsz - presz ~ presz, df)
b <- coef(m)[2]

d <- replicate(2000,
               {
                 s <- sample(df$presz)
                 n <- lm(postsz - presz ~ s, df)
                 coef(n)[2]
               })

hist(d, xlim = c(-1, 1) * 0.1)
abline(v = c(-1, 1) * b, col = 2, lwd = 2)

pVal = mean(abs(d) > abs(b))
pVal

# Since CV > pVal, reject H0
# i.e. population slope is NOT ONE
# probably closer to 0.91; given sample data


# challenge: repeat for the following
#
# H0: b - 0.92 = 0
# HA: b - 0.92 <> 0
#
# Should NOT reject H0



# Slide 25
# prediction CI
df <- read.csv('crabsmolt.csv')

rowCount <- nrow(df)

d <- replicate(2000,
               {
                 ind <- sample(1:rowCount,
                               replace = TRUE)
                 s <- df[ind, ]
                 m <- lm(postsz ~ presz, s)
                 coef(m)[2]
               })

quantile(d, c(0.025, 0.975))

# so true slope (that is population slope) is
# expect to be between [0.885, 0.943]
#
# Now zero is not in that interval
# So zero slope not a possibility


# Slide 28
df <- read.csv('crabsmolt.csv')

plot(residuals(m) ~ presz, data = df, pch = 19)
abline(h = 0)


# Slide 35
# prediction
df <- read.csv('crabsmolt.csv')

m <- lm(postsz ~ presz, df)
predict(m,
        newdata = data.frame(presz = c(0,
                                       120,
                                       140,
                                       160)))


# Slide 36
# Confidence Interval for the mean of a predicted value
df <- read.csv('crabsmolt.csv')

rowCount <- nrow(df)

d <- replicate(100,
               {
                 ind <- sample(1:rowCount,
                               replace = TRUE)
                 s <- df[ind, ]
                 m <- lm(postsz ~ presz, s)
                 
                 # Predict postsz given presz = 120
                 predict(m,
                         newdata = data.frame(presz = 120))
               })

quantile(d, c(0.025, 0.975))

# or
m <- lm(postsz ~ presz, df)
predict(m,
        newdata = data.frame(presz = 120),
        # interval = 'c' is required Confidence Interval 
        interval = 'c')

# So we have an interval for possible population
# prediction for a presz of 120
