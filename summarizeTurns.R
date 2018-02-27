turnDegTimeLocation <- function(data, turnIndex) { 
starts = turnIndex[[1]]; ends = turnIndex[[2]] 
turnEnd = numeric(); location = list(); time = list(); degrees = numeric()
for (i in 1:length(starts)) { 
	turnEnd  = ends[ends > starts[i]][1]
	location[[i]] = c(data[[4]][starts[i]], data[[3]][starts[i]], 
				data[[4]][turnEnd], data[[3]][turnEnd]) 
	time[[i]]     = c(data[[6]][starts[i]], data[[6]][turnEnd])
	degrees[i]  = data[[2]][turnEnd] - data[[2]][starts[i]]
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
return(list(location, time, degrees))
}