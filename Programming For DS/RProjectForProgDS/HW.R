library("ggplot2")


pos <- which.max(mtcars$mpg) 
mtcars[pos,]

times <- list(
  list(name = "Garry", age = 34, times  = c(45, 67, 56, 46, 78)),
  list(name = "Sue", age = 28, times = c(34, 57, 45, 46, 88, 57, 34)),
  list(name = "Pete", age = 25, times = c(76, 23)),
  list(name = "Jan", age = 41, times = c(90, 78, 67, 64, 57, 48, 47, 47))
)

cal <- function(l){
  return(mean(l$times) / l$age)
}
lapply(times, cal)

df <- data.frame(ID = c(580,625,630,721,700,855,740,187,571,312),
                 Subject = c("English", "English", "Science","Science",
                             "Maths", "English", "Science", "Science",
                             "English", "Maths"),
                 Mark = c(85,69,52,76,63,58,74,66,95,77))


df %>% group_by(Subject) %>% mutate(averageMarks = mean(Mark)) %>%
  select(averageMarks)
df %>% group_by(Subject) %>% summarise(averageMarks = mean(Mark, na.rm = TRUE))


# Student Surname: Mehndiratta
# Student First name: Mohit
# Student ID: 20622275
# Unit Name: Programming for Data Science
# Unit Number: 301113

## 1.1

# countdown <- 10
# while(countdown > 0){
#   print(countdown)
#   countdown <- countdown - 1
# }
# print("Start exam!")

## 1.2

# countdown <- 10
# while(countdown > -1){
#   cat("Time remaining:", countdown,"\n")
#   countdown <- countdown - 1
# }

## 1.3

# for(i in seq(from = 0, to = 1000, by = 100)){
#   cat("Time elapsed:", i,"\n")
# }

## 1,4
# 
# x <- 1
# while(x > 0){
#   x <- x + 1
#   print("Hello")
# }

## 2.1

# double <- function(x){
#   return (2*x)
# }
# 
# double(24)

## 2.2

# product <- function(x,y){
#   return(x*y)
# }
# 
# product(2,3)

## 2.3

# product_def <- function(x,y=2){
#   return(x*y)
# }
# 
# product_def(2)

## 2.4

## 3.1
# data <- read.csv("datasets/pfds.csv")
# head(data)
# print(data)
# 
# library("tidyverse")
# ## 3.2
# data %>% filter(back > 100)
# 
# ## 3.3
# 
# data %>% filter(back == max(back)) %>% select(front)

## 4.1

x <- 1:100
d <- data.frame(degree = x, inCount = ceiling(10000*x^{-2.2}),
                outCount = ceiling(10000*x^{-2.4}))

ggplot(data = d, mapping = aes(x = degree, y = inCount)) +
  geom_point() +
  labs(title = "Degree of In-Count", x = "Degree", y = "In Count")

## 4.2
ggplot(data = d, mapping = aes(x = degree, y = inCount)) +
  geom_point() + scale_x_log10() + scale_y_log10()

## 4.3 

ggplot(data = d, mapping = aes(x = degree, y = inCount)) +
  geom_point()+ scale_x_log10() + scale_y_log10() +
  geom_smooth()

ggplot(data = d, mapping = aes(x = inCount, y = outCount)) + 
  geom_point()


