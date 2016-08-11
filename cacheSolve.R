# The cacheSolve function takes the output of makeCacheMatrix as argument
# and returns the inverse of the argumet matrix to the makeCacheMatrix function.
# It will not calculate inverse if it has been once calculated for a given 
# argument and only return the previously calculated or cached value.

cacheSolve <- function(x, ...) {
  
# Storing the present value of inverse (latest return by makeCacheMatrix) at 'i'
  i <- x$getinverse()
#================================================================================
# if 'i' is not NULL, then cached or previously calculated value of 'i' 
# will be returned
  
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
#================================================================================ 
# Otherwise(i.e. if 'i' is NULL), calculate the inverse for the first time
# with the solve function and return
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}