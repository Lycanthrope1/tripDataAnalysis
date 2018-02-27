indexInInterval <- function(column, interval) { 
#Input: vector and upper and lowerbounds for acceptable elements
#
#Output: Index of elements within the specified interval
index = column < interval[2] & column >= interval[1]
return(index) 
}