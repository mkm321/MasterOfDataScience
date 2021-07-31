## This script file is for exploring Heart dataset.

# uploading the groceries dataset.
heartDataSet <- read.csv("datasets/heart.csv")
attach(heartDataSet)

# fetching top 6 enteries
head(heartDataSet,15)
# exploring data types of variables with sapply
sapply(heartDataSet, class)
# fetching dimensions of the dataset
dim(heartDataSet)
# summary of the dataset.
summary(heartDataSet)
# fetching the unique values in ChestPain variable.
unique(ChestPain)
# Box plot for Age vs AHD
boxplot(Age~AHD, data=heartDataSet)
# scatter plot for Age vs Chol to check the linearity between them.
plot(Age, Chol)
abline(lm(Chol ~ Age, data = heartDataSet), col = "blue")

