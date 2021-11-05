# Cheat sheet tidyverse - https://github.com/rstudio/cheatsheets/blob/master/data-transformation.pdf
## Maximum daily temperature at NY Airports.
## Compute maximum temperature using tidyverse function
library(tidyverse)
weather = read.csv("datasets/nycweather.csv")
max_temp <- weather %>% unite(date, year, month, day, sep = "-") %>% # combine column
            mutate(date = as.Date(date)) %>% # create a new column
            group_by(origin, date) %>% summarize(maxTemp = max(temp, na.rm = TRUE)) # reduce

max_temp_JFK <- max_temp %>% filter(origin == "JFK") # filtered results
max_temp_EWR <- max_temp %>% filter(origin == "EWR") # filtered results
max_temp_LGA <- max_temp %>% filter(origin == "LGA") # filtered results
head(max_temp_JFK)

pdf(file = "airportTemp.pdf", width = 5, height = 5)
plot(max_temp_EWR$date, max_temp_EWR$maxTemp, col = 6, xlab = "Date", ylab = "Max Daily temp", main = "Temp at NY Airports")
points(max_temp_JFK$date, max_temp_JFK$maxTemp, col = 4, pch = 2)
points(max_temp_LGA$date, max_temp_LGA$maxTemp, col = 3, pch = 3)
legend("topleft", legend = c("EWR", "JFK", "LGA"), col = c(6,4,3), pch = c(1,2,3))
dev.off()

## add a temperature using Degrees Celcius
temp_conversion <- function()

weather_temp <- max_temp %>% mutate(maxTempC = (maxTemp - 32)/1.8)
head(weather_temp)
