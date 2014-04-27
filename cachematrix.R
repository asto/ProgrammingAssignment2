## A couple of functions that cache the inverse of a matrix so that the cost of
## repetitive computation can be avoided

## makeCacheMatrix creates an object that can hold the inverse of a matrix.
##    get/set gets and sets the matrix
##    getinverse/setinverse gets and sets the inverse of the matrix set

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}


## cacheSolve returns the inverse of a matrix using the solve function. It
## caches the result in a makeCacheMatrix object

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
