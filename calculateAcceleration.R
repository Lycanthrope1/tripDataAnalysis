calculateAcceleration <- function(tripData, velocity) { 
#Input: trip data and velocity vector
#
#Output: acceleration vector  
#
#Initialize output
acceleration = 0
#Find dv / dt
for (i in 2:length(velocity)) { 
	acceleration[i] = velocity[i] - velocity[i - 1]
	acceleration[i] = acceleration[i] / (tripData$timestamp[i] - tripData$timestamp[i - 1])
}
return(acceleration)
} 
GPSAcceleration      = calculateAcceleration(data, GPSVelocity)
velocityAcceleration = calculateAcceleration(data, data$speed)
par(mfrow = c(1, 2)) 
plot(GPSAcceleration[GPSAcceleration < 10 & GPSAcceleration >= -10])
plot(velocityAcceleration[velocityAcceleration < 10 & velocityAcceleration >= -10])