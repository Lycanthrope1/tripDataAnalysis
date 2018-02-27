turnsGreaterThan <- function(turnSummary, threshold) { 
#Input: Turn summary data from turnDegTimeLocation; minimum acceptable turn angle 
#
#Output: Turn summary data with smaller turns excluded
print(turnSummary[,7])
#Determines which row meet threshold 
indexSufficingTurns = abs(turnSummary[,7]) > threshold
#Creates new summary using only those rows
newTurnSummary = numeric()
for (i in 1:length(turnSummary[1,])) { 
	newTurnSummary = cbind(newTurnSummary, turnSummary[,i][indexSufficingTurns])
} 
return(newTurnSummary)
} 