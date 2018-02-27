findMissingVelInterval <- function(data) { 
#Input: trip data 
#
#Output: missing intervals 
#(intervals starting with NA and ending with the first positive velocity)
#
#Finds NA values in speed data; these are the interval starting locations
breakPoints    = which(is.na(data$speed))
#Initializes output
indexIntervals = numeric()
#Finds each interval, indexes all of its points 
for (i in 1:length(breakPoints)) { 
	#Starting point for while loop which finds the first positive velocity 
	returnPoint = breakPoints[i] + 1
	#Finds numbers of points left that can be checked without exceeding vector length
	nPoints     = length(data$speed) - breakPoints[i] - 1 	
	while(returnPoint < nPoints & data$speed[returnPoint] == 0) { 
		returnPoint = returnPoint + 1
	}
	#Goes to the last zero value in the sequence starting with NA
	returnPoint = returnPoint - 1
	#Adds the indices of this sequence to those of any previous indices
	indexIntervals = c(indexIntervals, breakPoints[i]:returnPoint)
}
return(indexIntervals) 
}
 