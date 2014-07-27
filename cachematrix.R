## These companion functions cache the value of the inverse of a matrix to 
## a variable in a special environment rather than recomputing

## makeCacheMatrix is a list of four functions that can get/set the value of
## the inverse of the matrix and the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve checks the special environment and if the value is present,
## retreives it from the cache. If it's not present, it solves the matrix and
## puts the answer in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
