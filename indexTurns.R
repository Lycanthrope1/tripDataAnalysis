indexTurns <- function(headings) { 
#Input: headings vector
#
#Output: index of starting and ending elements for turns 
#
#straight; clockwise (cc); counterclockwise (ccw)
indexStarts = numeric(); indexEnds = numeric()
oldTurn = "straight"; newTurn = character() 
for (i in 2:length(headings)) { 
	#Determines angle change between successive elements
	degTurn = headings[i] - headings[i - 1]
	#If angle change is negative, but implausible in magnitude
	#Changes sign to account for going past 359 degrees; actually cc
	if (degTurn < -200) { 
		degTurn = 1
	}
	#If angle change is negative, but implausible in magnitude
	#Changes sign to account for going past 0; actually ccw
	if (degTurn > 200) { 
		degTurn = -1
	} 
	#Identifies presence and direction of turn
	if (degTurn == 0) { 
		newTurn = "straight"
	} 
	if (degTurn > 0) { 
		newTurn = "cc"
	} 
	if (degTurn < 0) { 
		newTurn = "ccw"
	} 	 
	#Identifies turn start as going from straight to turning
	#or changing turning direction 
	if (newTurn != oldTurn & newTurn != "straight") { 
		indexStarts = c(indexStarts, i - 1)  
		#Identifies turn end where start of new turn is end of old one
		if (oldTurn != "straight") { 
			indexEnds = c(indexEnds, i - 1)
		}		
	#Identifies going from turning to straight as end of turn 
	} else if (newTurn != oldTurn) { 
		indexEnds = c(indexEnds, i - 1)
	} 
	oldTurn = newTurn
} 
return(list(indexStarts, indexEnds)) 
} 
indexTurns(data$heading)