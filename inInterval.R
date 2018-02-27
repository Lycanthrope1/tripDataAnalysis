inInterval <- function(data, interval) { 
indexIn = data >= interval[1] & data < interval[2]
return(data[indexIn])
}