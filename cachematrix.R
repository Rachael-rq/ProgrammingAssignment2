## This script contains two functions that are able to make a matrix and cache the inverse ## of the matrix
## ---------------makeCacheMatrix-------------------
## makeCacheMatrix is a function that creates a special "matrix" object that can cache its ## inverse
## ---------------cacheSolve------------------------
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix ## above. If the inverse has already been calculated (and the matrix has not changed),then 
## the cachesolve should retrieve the inverse from the cache.

# makeCacheMatrix is a function that construct a matrix. It returns a list of functions for setting and accessing
# the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	if(is.null(x)){
		
	}
	setMatrix <- function(y){
	  x <<- y
	}
	getMatrix <- function() x 
	setInverse <- function(x = matrix()){
	  inverse <<- x
	}
	getInverse <- function() inverse
	
	list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve function caches the inverse of matrix x if not already cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  iMatrix <- x$getInverse()
  if(!is.null(iMatrix)) {
    message("getting cached data")
    return(iMatrix)
  }
  m <- x$getMatrix()
  if(dim(m)[1] != dim(m)[2]){
    warning("not a square matrix, can not cache inverse matrix")
    return()
  } #if the matrix is not square matrix
  iMatrix <- solve(m)
  x$setInverse(iMatrix)
  iMatrix
}
