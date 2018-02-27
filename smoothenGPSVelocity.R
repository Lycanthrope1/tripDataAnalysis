smoothenGPSVelocity <- function(GPSVelocity, meanInterval) { 
smoothedGPSVelocity = GPSVelocity[1:(meanInterval - 1)]
for(i in meanInterval:length(GPSVelocity)) { smoothedGPSVelocity[i] = mean(GPSVelocity[i:(i - (meanInterval - 1))]) } 
return(smoothedGPSVelocity)
}
par(mfrow = c(1,3))
smoothedGPSVelocity = smoothenGPSVelocity(GPSVelocity, 1)
plot(smoothedGPSVelocity[smoothedGPSVelocity < 100], data$speed[smoothedGPSVelocity < 100])
smoothedGPSVelocity = smoothenGPSVelocity(GPSVelocity, 2)
plot(smoothedGPSVelocity[smoothedGPSVelocity < 100], data$speed[smoothedGPSVelocity < 100])
smoothedGPSVelocity = smoothenGPSVelocity(GPSVelocity, 3)
plot(smoothedGPSVelocity[smoothedGPSVelocity < 100], data$speed[smoothedGPSVelocity < 100])
smoothedGPSVelocity = smoothenGPSVelocity(GPSVelocity, 5)
plot(smoothedGPSVelocity[smoothedGPSVelocity < 100], data$speed[smoothedGPSVelocity < 100])
smoothedGPSVelocity = smoothenGPSVelocity(GPSVelocity, 10)
plot(smoothedGPSVelocity[smoothedGPSVelocity < 100], data$speed[smoothedGPSVelocity < 100])
smoothedGPSVelocity = smoothenGPSVelocity(GPSVelocity, 20)
plot(smoothedGPSVelocity[smoothedGPSVelocity < 100], data$speed[smoothedGPSVelocity < 100]) 
smoothedGPSVelocity = smoothenGPSVelocity(GPSVelocity, 100)
plot(smoothedGPSVelocity[smoothedGPSVelocity < 100], data$speed[smoothedGPSVelocity < 100])

