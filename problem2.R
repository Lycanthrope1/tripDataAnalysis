# Mock function
install.packages("dplyr")
library(dplyr)
sample_trip = readRDS('sample_trip.rds')

get_acceleration = function(time_vect, speed_vect, lag = 1) {
  dt = diff(time_vect, lag = lag)
  dv = diff(speed_vect, lag = lag)
  accel = dv/dt
  #Fixes lag 2 problem experienced in the summarize syntax
  accel = accel[accel !=Inf & !is.na(accel) & !is.nan(accel)]
  return(accel)
}

accuracy_vect = sample_trip$accuracy
time_vect     = sample_trip$timestamp 
speed_vect    = sample_trip$speed
fix_speed = function(time_vect, speed_vect, 
                     accuracy_vect, accuracy_thresh = 25) {
  bad_accuracy = accuracy_vect > accuracy_thresh
  #Bad code; between the first and last inaccurate rows, there are accurate rows
  #
  #Also, you can simply get rid of the inaccuracy on a row by row basis.
  #
  #If many consecutive rows are missing, interpolation is wrong and the data is best discarded. 
  #
  #If individual rows are missing here and there, the effect of throwing them out is minimal.
  #
  #In the case of speed, speed from the GPS data appears to act normally when accuracy is 50 
  #and the provided speed vector is missing data (NA,0,0,...).  Likey, the accuracy metric itself 
  #is glitched in some circumstances.
  #
  #While it bears further investigation, substituting intervals of one speed vector for another 
  #when possible, and discarding the data when it isn't seems like a better strategy than assuming
  #knowledge of the missing interval.  In my problem 1 implementation, I still used an accuracy filter
  #to be conservative.
  #
  #Hypothesis: Whatever made speed glitch also made accuracy glitch, but did not glitch GPS coordinates. 
  start_bad = min(which(bad_accuracy))
  end_bad = max(which(bad_accuracy))
  new_speed = approx(x = time_vect[c(start_bad-1, end_bad+1)],
                     y = speed_vect[c(start_bad-1, end_bad+1)],
                     xout = time_vect[bad_accuracy])$y
  speed_vect[bad_accuracy] = new_speed      	
  return(speed_vect)
}

data = sample_trip
GPSVelocity = calculateGPSVelocity(data)
#Finds interval beginning with NA and ending with the first positive velocity after NA 
missingIntervals = findMissingVelInterval(data)
#Fills in missing "speed" data with GPS Velocity
data$speed[missingInterval] = GPSVelocity[missingInterval]
sample_trip = sample_trip %>% 
  mutate(recovered_speed = data$speed)
par(mfrow = c(1, 3))
plot(sample_trip$speed)
plot(sample_trip$recovered_speed) 
plot(sample_trip$accuracy)

summary = sample_trip %>%
  summarize(max_speed = max(recovered_speed),
            max_accel_lag1 = get_acceleration(timestamp, recovered_speed, lag = 1) %>% max,
		#This broke because sample_trip$timestamp[400] = sample_trip$timestamp[402]
		#This leads to dividing by zero in the acceleration function (which in R is Inf).  
		#
		#My code was immune to this because it filters out accelerations above 1 g. 
            max_accel_lag2 = get_acceleration(timestamp, recovered_speed, lag = 2) %>% max,
            max_accel_lag3 = get_acceleration(timestamp, recovered_speed, lag = 3) %>% max)

print(summary)

