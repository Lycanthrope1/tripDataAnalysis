applyFilters <- function(data, listInts) { 
for (i in 1:length(listInts)) { 
	data = filterData(data, data[[i]], listInts[[i]])
} 
return(filteredData) 
} 
