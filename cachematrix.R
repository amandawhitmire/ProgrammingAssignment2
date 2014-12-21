# # # # # # # # # # # # # # # # # # # # # 
# This script creates two functions that accomplish the following:
#   1. The first function, "makeCacheMatrix" creates a special "matrix", 
#   which is really a list containing a function to:
#     set the value of the matrix
#     get the value of the matrix
#     set the value of the inverse
#     get the value of the inverse
#   2. The second function, "cacheSolve" calculates the mean of the 
#   special "matrix" created with the above function. However, it first 
#   checks to see if the mean has already been calculated. If so, it 
#   gets the mean from the cache and skips the computation. Otherwise, 
#   it calculates the mean of the data and sets the value of the mean 
#   in the cache via the setmean function.
# 
# This is an assignment for the JHU/Coursera R Programming course
# # # # # # # # # # # # # # # # # # # # # 

## This function creates a special "matrix" object that can 
# cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  setmatrix <- function(y) {
    x <<- y # caches the input matrix
    m <<- NULL #sets value of m to NULL
  }
  getmatrix <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already 
# been calculated (and the matrix has not changed), then the 
# cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) { # check to see if cacheSolve has been run before
    message("getting cached data")
    return(m)
  }
  # otherwise 
  data <- x$getmatrix()
  m <- solve(data, ...) 
  x$setinverse(m) 
}