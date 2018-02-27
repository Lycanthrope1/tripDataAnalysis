calcTripTime <- function(data, velocity) {
#Input: Trip Data and a velocity vector
#
#Output: Trip time in seconds 
#
#Find the first positive velocity 
count = 1; checkZero = TRUE; indexStart = numeric()
while (count < length(velocity) & checkZero) { 
	checkZero  = velocity[count] == 0  
	count      = count + 1
	indexStart = count - 1
}
#Find the last positive velocity
reverseTime = rev(velocity)
count = 1; checkZero = TRUE; indexEnd = numeric()
while (count < length(reverseTime) & checkZero) { 
	checkZero  = reverseTime[count] == 0  
	count      = count + 1
	indexEnd   = count
}
indexEnd = length(data$speed) - indexEnd
#Subtract the time stamp of the final positive velocity from that 
#of the final negative velocity
tripTime = data$timestamp[indexEnd] - data$timestamp[indexStart]
return(tripTime)
}
