## This script file is for exploring Heart dataset.

# uploading the groceries dataset.
irisDataSet <- read.csv("datasets/iris.csv")
attach(irisDataSet)
# fetching top 6 enteries
head(irisDataSet)
# exploring data types of variables with sapply
sapply(irisDataSet, class)
# fetching dimensions of the dataset
dim(irisDataSet)
# summary of the dataset.
summary(irisDataSet)
# fetching top 15 enteries
head(irisDataSet, 15)

# box plots for different relations with respect to types
boxplot(SL~Type, data=irisDataSet)
boxplot(SW~Type, data=irisDataSet)
boxplot(PL~Type, data=irisDataSet)
boxplot(PW~Type, data=irisDataSet)
