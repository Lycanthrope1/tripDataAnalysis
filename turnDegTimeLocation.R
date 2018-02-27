turnDegTimeLocation <- function(data, turnIndex) { 
#Input: trip data (prefiltered to remove 0 velocity rows) and index of turns 
#
#Output: Array where columns are initialLatitude, initialLongitude, 
#					   finalLatitude, finalLongitude,
#			               initialTime, finalTime, degrees
starts = turnIndex[[1]]; ends = turnIndex[[2]] 
#Initial ouputs 
turnEnd = numeric(); initialLatitude = numeric(); initialLongitude = numeric() 
finalLatitude = numeric(); finalLongitude = numeric()
initialTime = numeric(); finalTime = numeric(); degrees = numeric()
for (i in 1:length(starts)) { 
	#For each turn start, find corresponding turn end 
	turnEnd  = ends[ends > starts[i]][1]
	#Place data corresponding to start and end into output vectors
	initialLatitude[i] = data[[3]][starts[i]]
	initialLongitude[i] = data[[4]][starts[i]]
	finalLatitude[i] =  data[[3]][turnEnd]
	finalLongitude[i] = data[[4]][turnEnd]
	initialTime[i] = data[[6]][starts[i]]
	finalTime[i] = data[[6]][turnEnd]
	degrees[i]  = data[[2]][turnEnd] - data[[2]][starts[i]]
	#If turn goes from 359 to 0, compensates
	#Next iteration would take into account large turns that cross 0 line
	if (degrees[i] > 200) { #counter clockwise; negative
		sumTurn    = 0
		for (j in (starts[i] + 1):turnEnd) { 
			headingDiff = data[[2]][j] - data[[2]][j - 1]
			if (headingDiff > 0) { 
				headingDiff = headingDiff - 360
			} 
			sumTurn = sumTurn + headingDiff 
		}             
		degrees[i] = sumTurn 
	}
	#If turn goes from 0 to 359, compensates
	if (degrees[i] < -200) { #clockwise; positive
		sumTurn    = 0
		for (j in (starts[i] + 1):turnEnd) { 
			headingDiff = data[[2]][j] - data[[2]][j - 1]
			if (headingDiff < 0) { 
				headingDiff = headingDiff + 360
			} 
			sumTurn = sumTurn + headingDiff 
		}             
		degrees[i] = sumTurn 
	}
}
return(cbind(initialLatitude, initialLongitude, finalLatitude, finalLongitude,
            initialTime, finalTime, degrees))
}