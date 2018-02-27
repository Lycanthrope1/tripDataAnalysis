calculateGPSVelocity <- function(tripData) { 
#Input: tripData (list); Output: velocity vector from GPS and timestamp data
#
#Initialize output
GPSVelocity = 0
#Uses geosphere package to calculate distance between successive GPS coordinates
#Combined with timestamp data can be used to determine velocity: distance / time 
for (i in 2:length(tripData$speed)) { 
	GPSVelocity[i] = distm(c(tripData$longitude[i], tripData$latitude[i]), 
					c(tripData$longitude[i - 1], tripData$latitude[i - 1])) 
	GPSVelocity[i] = GPSVelocity[i] / (tripData$timestamp[i] - tripData$timestamp[i - 1])
}
return(GPSVelocity)
}
GPSVelocity = calculateGPSVelocity(data)