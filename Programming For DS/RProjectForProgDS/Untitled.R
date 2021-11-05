weather = read.csv("datasets/nycweather.csv")
airports <- unique(weather$origin)
airportWeather <- list()
## fetch using for loop
for(airport in airports){
  airportWeather[[airport]] <- subset(weather, subset = (origin == airport))
}
names(airportWeather)
## fetch Using Lapply
fetchAirport <- function(airport, weather) {
  return(subset(weather, subset = (origin == airport)))
}

airportWeather <- lapply(airports, fetchAirport, weather)
names(airportWeather) <- airports
airportWeather[["JFK"]]
######

# Merging columns for the date.
dateString <-  paste(weather$year, weather$month, weather$day, sep = "-")
weatherDate <- as.Date(dateString)
weatherDate[1]

for(airport in airports){
  dateString <-  paste(airportWeather[[airport]]$year, airportWeather[[airport]]$month, 
                       airportWeather[[airport]]$day, sep = "-")
  weatherDate <- as.Date(dateString)
  airportWeather[[airport]]$date <- weatherDate
}

## summarise the max temperature and group it by date.
## compute max temp for each day

computeMaxTemp <- function(weather) {
  return(tapply(weather$temp, weather$date, max))
}
maxTempData <- lapply(airportWeather, computeMaxTemp)
maxTempData[["JFK"]]

####
computeMaxTemp <- function(weather) {
  max_temp <- tapply(weather$temp, weather$date, max)
  date_temp <- as.Date(names(max_temp))
  df <- data.frame(date = date_temp, temp = max_temp)
  return(df)
}
maxTempData <- lapply(airportWeather, computeMaxTemp)
plot(maxTempData[["JFK"]]$date, maxTempData[["JFK"]]$temp, col = 1)
points(maxTempData[["EWR"]]$date, maxTempData[["JFK"]]$temp, col = 2)
points(maxTempData[["LGA"]]$date, maxTempData[["JFK"]]$temp, col = 3)

