sedtwd("C:/Users/Bear/Documents/R/RootsAssignment")
csvPath = "sample_trip.csv"; rdsPath = "sample_trip.rds"
data = readRDS(rdsPath);
par(mfrow=c(2,3))
for (i in 1:length(data)) { plot(data[[i]], pch = 20, xlab = "Index  Row", ylab = names(data[i]) ) }
acceleration = 0
for (i in 2:length(data$speed)) { acceleration[i] = data$speed[i] - data$speed[i - 1] }
par(mfrow=c(1,1))
plot(acceleration,xlab = "Index Row", ylab = "Acceleration") 

findNa <- function(x) {  
	return(paste("Isn't numeric?",which(!is.numeric(x)), "Isn't finite?",which(!is.finite(x))))
}
lapply(data, findNa)
data$speed[899]

install.packages("geosphere")
library(geosphere)
GPSVelocity = 0
for (i in 2:length(data$speed)) { 
	GPSVelocity[i] = distm(c(data$longitude[i], data$latitude[i]), c(data$longitude[i - 1], data$latitude[i - 1])) 
}
par(mfrow= c(1,2))
plot(GPSVelocity[GPSVelocity < 100],xlab = "Index Row", ylab = "Velocity from GPS") 
plot(data$speed,xlab = "Index Row", ylab = "Velocity") 
par(mfrow= c(1,1))
plot(GPSVelocity[GPSVelocity < 100], data$speed[GPSVelocity < 100], ylab = "Velocity", xlab = "Velocity from GPS") 
abline(lsfit(GPSVelocity[GPSVelocity < 100] , GPSVelocity[GPSVelocity < 100]), col="red")


par(mfrow = c(1,1))
plot(GPSVelocity[GPSVelocity < 100], data$speed[GPSVelocity < 100], xlab = "Velocity", ylab = "Velocity from GPS") 
par(mfrow = c(1,2))
diffVelocity = data$speed[GPSVelocity < 100] - GPSVelocity[GPSVelocity < 100]
plot(data$accuracy[GPSVelocity < 100], diffVelocity)
plot(data$accuracy[GPSVelocity < 100][diffVelocity > -10], diffVelocity[diffVelocity > -10])
plot(diffVelocity / data$speed[GPSVelocity < 100], data$accuracy[GPSVelocity < 100])

inclusionIndex = data$accuracy != 50 & GPSVelocity < 100
dataWOBlip = list()
for (i in 1:length(data)) { dataWOBlip[[i]] = data[[i]][inclusionIndex]  }  

constantAccurracyIndex =  list()
inclusionIndex = data$accuracy != 50 && GPSVelocity < 100
  
for (i in 1:11) {
	 
 } 

rootAssignmentProblem1 <- function(rdsPath) { 
data = readRDS(rdsPath);

findEnd()
includeIndexed()
findDuration() 
 
} 