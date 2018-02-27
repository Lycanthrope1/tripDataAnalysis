indexInInterval <- function(column, interval) { 
index = column < interval[2] & column >= interval[1]
return(index) 
}