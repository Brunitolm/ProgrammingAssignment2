## Functions able to cache potentially time-consuming computations
## Matrix inversion is used to cache the inverse of the matrix rather
## than compute it repeatedly

## the function creates a matrix object with the ability to cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function( y ) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setInverse <- function() inv <<- solve(x)
  getInverse <- function () inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## the cacheSolve function is able to compute the inverse of the special matrix
## calculated by the makeCacheMatrix 

cacheSolve <- function(x, ...) {
        
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat<- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}
  
