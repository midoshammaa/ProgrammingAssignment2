## This script contains two functions 
## the first function creates a special matrix object and the second one
## solves for an inverse of the matrix and caches the solution
## as long as the matrix has not change there would be no need for 
## a recalculation.

## This function accepts a regualr matrix and creates a special matrix
## the special matrix has 4 methods set, get and setinv and getinv
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function accepts a special matrix object and if the inverse exists in 
## the cached memory it does not recompute it.  Otherwise it gets the data
## and recalculates it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m

}
