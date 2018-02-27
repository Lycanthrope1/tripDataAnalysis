writeSummaryCSVs <- function(genSummary, turnSummary) { 
#Inputs: General Summary data frame; turn summary data (in array)  
#
#Output: Creates csvs that contain turn and general summart statistics
turns = data.frame(turnSummary[,1], turnSummary[,2], turnSummary[,3], turnSummary[,4],
	             turnSummary[,5], turnSummary[,6], abs(turnSummary[,7]))   
colnames(turns) = c("Initial Latitude (degrees)","Initial Longitude (degrees)", 
			  "Final Latitude (degrees)", "Final Longitude (degrees)",
			  "Initial Timestamp (s)", "Final Timestamp (s)", 
			  "Angle Turned (degrees)")
write.csv(turns, "Turn Summaries.csv", row.names = FALSE)
colnames(genSummary) = c("Trip Duration (s)", "Trip Distance Speed-Method (miles)", 
			"Trip Distance GPS-Method (miles)", "Number of 60 Degree Turns")
write.csv(genSummary, "Trip General Summary.csv",row.names = FALSE)
} 
