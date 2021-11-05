library(dplyr)
library("ggplot2")
library(psych)
# 1. Import the abalone dataset and explore.
abalone <- read.csv("datasets/abalone.csv", stringsAsFactors = TRUE)
head(abalone)
dim(abalone)
summary(abalone)
str(abalone)
# 2. Subset the dataset by selecting the variables Length, Diameter and Height. 
new_data <- abalone %>% select(Length, Diameter, Height)
head(new_data)
# 3. Subset the dataset by selecting observations of female abalone.
female <- abalone %>% filter(Sex == "F")
head(female)
# 4. Subset the dataset by selecting the weight variables of Male abalone.
male_weight <- abalone %>% filter(Sex == "M") %>% select(Sex, Whole.weight,
                                                         Shucked.weight,
                                                         Viscera.weight,
                                                         Shell.weight)
head(male_weight)
# 5. Summarize the mean-length by gender

mean_length <- abalone %>% group_by(Sex) %>% summarise(mean = mean(Length))
mean_length


### Question 2



## 1. Create a scatter plot of matrices for the dataset.

pairs.panels(abalone)
## 2. Create a scatter plot of Diameter Vs Length

ggplot(data = abalone, mapping = aes(Length, Diameter)) + geom_point(col = "blue")

## 3. Create a set of histograms of Shell-weight by Sex category 
## and fill with different colurs. (use facet_wrap with 1 column)

ggplot(data=abalone, mapping=aes( x=Shell.weight, fill=Sex)) +
  geom_histogram()+
  facet_wrap(~Sex, ncol=1)
## 4. Create violin plot for Shell-weight by Sex category.

ggplot(data = abalone, mapping = aes(x = Sex, y = Shell.weight, fill = Sex)) +
  geom_violin()

## 5. Create box plot for Shell-weight by Sex category. 
## Enhance the plot by adding title, labels etc. . .

ggplot(data = abalone, mapping = aes(x = Sex, y = Shell.weight, fill = Sex)) +
  geom_boxplot() + 
  labs(title = "Boxplots for Sex Categories w.r.t Shell-Weight", x = "Sex"
       , y = "Shell Weight")
