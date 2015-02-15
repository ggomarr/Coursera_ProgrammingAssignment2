## These functions define an 'ubermatrix' object that allows the inverse of a matrix to be cached
## for quick recall.

## makeCacheMatrix takes a matrix and initializes the 'ubermatrix' object (which contains the
## matrix, a placeholder for its inverse, and functions to change and set both the matrix and its
## inverse).

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(cache_inv) inv <<- cache_inv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve returns the inverse of the 'ubermatrix' defined above. If the inverse has not been
## requested before, it calculates it and stores it in the allocated space within the 'ubermatrix'
## object, and if it has been requested before it just retrieves it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
