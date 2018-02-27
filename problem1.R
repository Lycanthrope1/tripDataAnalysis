problem1 <- function (firstTime = TRUE, tripDataPath = "C:/Users/Bear/Documents/R/RootsAssignment/sample_trip.rds") { 
#Input: binary variable firstTime, indicates whether used packages need to be installed
#       string variable tripDataPath, tells R where to find your trip data
#
#
#Install Used Libraries; 
if (firstTime) { 
	installLibraries()
}
#Load used Libraries
#Load the data

loadLibraries()
#Go to folder where trip data is stored
setwd(tripDataPath)
data = readRDS("sample_trip.rds");
#Calculate velocity from GPS coordinates: (position2 - position1) / (time2 - time1) 
GPSVelocity = calculateGPSVelocity(data)
#Finds interval beginning with NA and ending with the first positive velocity after NA 
missingIntervals = findMissingVelInterval(data)
#Fills in missing "speed" data with GPS Velocity
data$speed[missingInterval] = GPSVelocity[missingInterval]
#Calculates the implied acceleration from GPS and "speed" respectively
GPSAcceleration = calculateAcceleration(data, GPSVelocity)
velocityAcceleration = calculateAcceleration(data, data$speed)
#Acceleration < 1 g; Acceleration > -g;  plausbility tests from research papers and tesla model s stats  
#Accuracy < 5 m; a bit arbitrary; would  want to test effect of filter on machine learning algorithms
#Velocity > 0 m / s; Velocity < 70 m / s; assumes driver won't exceed 156 mph
GPSIndex = indexInInterval(GPSAcceleration, c(-5, 9.8)) & indexInInterval(data$accuracy, c(0, 5)) & indexInInterval(GPSVelocity, c(0, 70))   
velIndex = indexInInterval(velocityAcceleration, c(-5, 9.8)) & indexInInterval(data$accuracy, c(0, 5)) & indexInInterval(data$speed, c(0, 70))
#Calculates trip time from the first to last positive "speed" values 
tripDuration = calcTripTime(data, data$speed)
#Calculates distance traveled by both GPS Velocity and "speed" methodologies 
velDist = tripDistance(data, data$speed, velIndex)
GPSDist = tripDistance(data, GPSVelocity, GPSIndex)
#When the car is stopped; the heading can be measured; so data where the heading is zero is removed 
turnData = filterData(data, data$speed, c(0.000001, 360)) 
#Finds where all turns begin and end 
turnIndex = indexTurns(turnData[[2]])
#Records the starting and stopping location and times 
turnSummary = turnDegTimeLocation(turnData, turnIndex)
#Removes turns that are less than 60 degrees
turnSummary = turnsGreaterThan(turnSummary, 60)
#Counts those turns
count = length(turnSummary[,1])
#Places general stats into a data frame 
genSummary = data.frame(TripDuration = tripDuration, DistanceFromSpeed = velDist, 
		DistanceFromGPS = GPSDist, NumberOf60DegTurns = count)
genSummary = data.frame(tripDuration, velDist, GPSDist, count)
#Converts turn-data into data frame; writes CSVs containing general and turning states
writeSummaryCSVs(genSummary, turnSummary) 
}