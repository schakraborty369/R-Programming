# The function makeCacheMatrix takes a matrix as argument and creates a
# special 'matrix' which infact is a list containing a function that will
# a) set the value of the matrix
# b) get the value of the matrix
# c) set the value of the inverse of the argument matrix
# d) get the value of the inverse of the argument matrix

makeCacheMatrix <- function(x = matrix()) {
#==========================================================================
# setting 'i' to NULL as placeholder for its future values
  i <<- NULL
#==========================================================================
# Setting the matrix x, to a new matrix, y, and resets 'i' to NULL  
  set <- function (y) {
    x <<- y
    i <<- NULL
  }
#==========================================================================
# Returning the set matrix x
  get <- function() x
#==========================================================================
# Setting the value of 'i' to inverse
  setinverse <- function(inverse) i <<- inverse
#==========================================================================
# Getting the value of 'i' that was set 
  getinverse <- function() i
#==========================================================================
# Returning the list by the main function makeCacheMatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
}