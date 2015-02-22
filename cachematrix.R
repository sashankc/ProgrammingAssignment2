## Assignment 2 - Caching the Inverse of a Matrix
## Author - Sashank T.
##
## This file caculates an inverse of a matrix effeciently
## It would first try to locate if an inverse value for a matrix exists in the cache
## If no vlaue is foudn it would create the inverse and store it in the cache.
## this program contains 2 functions, makeCacheMatrix and cacheSolve. 
##

## Function - makeCacheMatrix
## This function is responsible for maintaining the cache for inverse values
## The get  inner function which would return a inverse value for a matrix
## set inner function sets a matrix inverse value in the cache.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}

## Function - cacheSolve
## This function is called to solve the matrix inverse, 
## it checks if an inverse value for is available in the cache and retrieves it 
## if not it calculates  the inverse and calls the setinverse function to put it in the cache 
cacheSolve <- function(x, ...) {
  ## looks up the inverse value
  m <- x$getinverse()
  if(!is.null(m)) { 
    message("getting cached data")
  ## Return the value from cache
    return(m)
  }
## calculate the inverse and set it to the cache
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
