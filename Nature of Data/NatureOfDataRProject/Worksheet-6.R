birthwt = read.csv("datasets/birthwt.csv")
head(birthwt, n=10)
birthwt <- birthwt[, 1:2]
attach(birthwt)
dim(birthwt)
nrow(birthwt)
ncol(birthwt)
names(birthwt)
smoke_data <- table(birthwt$smoke)
barplot(smoke_data)
## mean, med, var
mean(birthwt$bwt)
median(birthwt$bwt)
var(birthwt$bwt)
sd(birthwt$bwt)
# 3. Calculate the min, max of `bwt` and from it the range.
min(birthwt$bwt)
max(birthwt$bwt)
range(birthwt$bwt)
diff(range(bwt))
# Quantile
quantile(birthwt$bwt, 1/4)
quantile(bwt, 2/4)
quantile(bwt, 3/4)

## Compare the output given for the second quartile to the median
a <- median(bwt)
b <- quantile(bwt,2/4)
a-b
##
IQR(bwt)
##
summary(bwt)
## Histograms
hist(birthwt$bwt)
hist(birthwt$bwt, xlab="Birth Weight", main="", col="lightblue")
hist(birthwt$bwt, xlab="Birth Weight", main="", col="lightblue", breaks=20)

## Splitting the data

aggregate(bwt~smoke, birthwt, mean)
aggregate(bwt~smoke, birthwt, summary)

## 7. Replace mean in the aggregate statement to get a spit by median and sd.
aggregate(bwt~smoke, birthwt, median)
aggregate(bwt~smoke, birthwt, sd)

## Split charts

library(lattice)
histogram(~bwt|smoke, data=birthwt)

## Box Plots
boxplot(birthwt$bwt)
boxplot(birthwt$bwt, horizontal=TRUE, pch=16)
boxplot(bwt ~smoke, data=birthwt)

## Difference in mean
diff(aggregate(bwt~smoke, birthwt, mean)$bwt)
mm <- -diff(aggregate(bwt~smoke, birthwt, mean)$bwt)
set.seed(12345)
smoke.sim = sample(birthwt$smoke)
-diff(aggregate(bwt~smoke.sim, birthwt, mean)$bwt)

# replicate 1000
d1 <- replicate(1000, {
  smoke.sim = sample(birthwt$smoke)
  -diff(aggregate(bwt~smoke.sim, birthwt, mean)$bwt)
})
hist(d1, xlim = c(-100,300))
abline(v = mm, col="red")
# replicate 10000
d2 <- replicate(10000, {
  smoke.sim = sample(birthwt$smoke)
  -diff(aggregate(bwt~smoke.sim, birthwt, mean)$bwt)
})
hist(d2, xlim = c(-100,300))
abline(v = mm, col="red")

## Here we reject the null hypothesis


## Sales
## H0 : mean(east) = mean(west)
##$H_0$ : $\mu_{east} = \mu_{west}$
##$H_A$ : $\mu_{east} \neq \mu_{west}$

sales_df <- read.csv("salesEW.csv")
attach(sales_df)
head(sales_df)
sales_df <- sales_df[,1:2]

result <- diff(aggregate(sales~office, sales_df, mean)$sales)
result ## west - east
result1 <- -result ## east - west

## we want both of them here, west - east and east -  west.

d <- replicate(1000, {
  officeShuffle <- sample(office)
  diff(aggregate(sales~officeShuffle, sales_df, mean)$sales)
})
hist(d)
abline(v = result, col="blue") # west - east
abline(v = result1, col="red") # east - west

## to find the p-value
mean(d > result1) + mean(d < result) # and it should be less than for our critical value 0.05

## we can't rejject the null hypothesis because it is greater than 0.05
## we do not have enough evidence
## the alternantive is not true
## they are likely to be the same mean.

## SPIDER
spider_df <- read.csv("Spider.csv")
head(spider_df)
spider_df <- spider_df[,1:2]
attach(spider_df)

## h0: mean picture = mean real
## ha: mean picture < mean real
      # mean picture - mean real < 0

anx <- diff(aggregate(Anxiety~Group, spider_df, mean)$Anxiety)
anx # this is real - picture ## how do we check if its real - picture and not picture - real
## simply, diff follows alphabetic order so greater alphabet - lower alphabet
anx1 <- -anx # this is picture - real

d <- replicate(1000, {
  groupShuffle <- sample(Group)
  -diff(aggregate(Anxiety~groupShuffle, spider_df, mean)$Anxiety)
})
hist(d)
abline(v = anx1, col = "red")


## to find the p-value
mean(d < anx1) # and it should be less than for our critical value 0.05

## since it is less than critical value we can reject the null hypothesis.
