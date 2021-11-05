library("ggplot2")

ggplot(data = mpg) + 
  geom_histogram(mapping = aes(x = cty))
head(mpg)


# class problem

k <- ggplot(data = mpg) + 
  geom_histogram(mapping = aes(x = hwy, fill = drv), bins = 10) + 
  labs(title = "Distribution of fuel used for highway driving",
       x = "Miles per gallon", y = "Count") +
  theme(legend.position="bottom") + facet_wrap(~cyl)

ggsave("highway.pdf", plot = k)
