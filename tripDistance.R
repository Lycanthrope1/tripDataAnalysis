tripDistance <- function(data, velocity, index) {
#Input: trip data, velocity vector, index of applicable rows 
#
#Output: Trip distance in miles
#
#Strip out unused rows 
velocity = velocity[index]  
times    = data$timestamp[index] 
#Initialize output
distance = 0
#Calculates distance between points as average of beginning and ending velocity 
#divided by time difference between the
for (i in 2:length(velocity)) { 
	avgVelocity = (velocity[i] + velocity[i - 1]) / 2  
	timeDiff    = times[i] - times[i - 1] 
	distance = distance + (avgVelocity * timeDiff)  
}
#Converts meters to miles
distance = distance * 0.000621371
return(distance) 
}