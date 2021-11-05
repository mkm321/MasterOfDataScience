birthwt <- read.csv("datasets/birthwt.csv")
head(birthwt)

str(birthwt)
table(birthwt$smoke)
barplot(table(birthwt$smoke))

bwt.smoke <- subset(birthwt, smoke == "yes", bwt, drop = TRUE)
bwt.nonsmoke <- subset(birthwt, smoke == "no", bwt, drop = TRUE)
sum(outer(bwt.smoke, bwt.nonsmoke, "<"))

wilcox.test(bwt~smoke, birthwt, alternative="greater")
