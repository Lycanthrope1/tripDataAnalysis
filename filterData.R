filterData <- function(data, column, interval) { 
#Input: trip data, vector of matching length, c(lower bound, upper bound)
# 
#Output: data excluding rows with values outside of bounds for the interval
index = column < interval[2] & column >= interval[1]
filteredData = list()
for (i in 1:length(data)) { filteredData[[i]] = data[[i]][index]  } 
return(filteredData) 
}
par(mfrow = c(1,2)) 
GPSData               = filterData(data, GPSAcceleration, c(-5, 9.8))
GPSAcceleration2      = filterData(GPSAcceleration, GPSAcceleration, c(-5, 9.8))
velocityData          = filterData(data, velocityAcceleration, c(-5, 9.8))
velocityAcceleration2 = filterData(velocityAcceleration, velocityAcceleration, c(-5, 9.8))

