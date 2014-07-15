############################################################################
## Summary
##
## Function name: makeCacheMatrix
##
## General description: This function creates a special "matrix" object that 
##                      can cache its inverse.
##    
## Input: A square invertible matrix
##
## Retuen: A list of functions which set and get a matrix with its inverse.
##
############################################################################

makeCacheMatrix <- function(x = matrix()) {
  
  # private variable with initial NULL
  inv <- NULL
  
  # function to set private variable x and update inv with NULL
  setmatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # function to get x
  getmatrix <- function() x
  
  # function to set inv
  setinverse <- function(inverse) inv <<- inverse
  
  # function to get inv
  getinverse <- function() inv
  
  # return a list of functions
  list("setmatrix" = setmatrix, "getmatrix" = getmatrix,
       "setinverse" = setinverse, "getinverse" = getinverse)
  
}

############################################################################
## Summary
##
## Function name: cacheSolve
##
## General description: This function computes the inverse of the 
##                      special "matrix" returned by makeCacheMatrix. 
##                      If the inverse has already been calculated
##                      and the matrix has not changed, then the cachesolve
##                      should retrieve the inverse from the cache.
##
## Input: an instance of makeCacheMatrix class
## 
## Return: the inverse matrix of input
## 
############################################################################

cacheSolve <- function(x) {
  
  # get the inv of x
  inv <- x$getinverse()
  
  # if inv is not NULL, return the cach
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # inv is NULL, calculate the inverse of x
  inv <- solve(x$getmatrix())
  
  # set inv
  x$setinverse(inv)
  
  # return inv
  inv
  
}