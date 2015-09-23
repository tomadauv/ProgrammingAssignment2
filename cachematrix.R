## This script is composed of two functions that will prevent useless computation of the inverse of a matrix
## if it has already been computed before and use the cache to store the results.

## This function named 'makeCacheMatrix' create a list of functions to set and get the initial matrix to be inverted
## and two other functions to set and get the value of the inverted matrix

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
          								  setinv = setinv, getinv = getinv)

}


## This function named 'cacheSolve' checks if the inverse of the matrix has already been calculated and if it's the case diplay
## the result retrieved from the cache and a message 'getting cached data'. If it is not the case it solves the matrix.

cacheSolve <- function(x, ...) {
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
