## This script file is for exploring Groceries dataset.

# uploading the groceries dataset.
groceriesDataSet <- read.csv("datasets/groceries.csv")
attach(groceriesDataSet)

# fetching top 6 enteries
head(groceriesDataSet)

# exploring data types of variables with sapply
sapply(groceriesDataSet, class)
# fetching dimensions of the dataset
dim(groceriesDataSet)
# summary of the dataset.
summary(groceriesDataSet)

## logic for getting top 10 bought products

# since data for each and every variable is in 0s and 1s therefore 
# adding all of them to know the exact amount of bought items.
accumulatedTotal <- colSums(groceriesDataSet[,-1])
# sorting the items from highest to lowest
accumulatedTotalDescending = sort(accumulatedTotal, decreasing = TRUE)
# fetching top 10 items
accumulatedTotalDescending[0:10]
# plotting the top 10 items.
barplot(accumulatedTotalDescending[0:10], las=2)
