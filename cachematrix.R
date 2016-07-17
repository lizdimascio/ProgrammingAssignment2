## This is a pair of functions that caches the inverse of a matrix
## in order for it to be used repeatedly in other functions or
## computations. Because matrix inversion is usually a costly
## computation, there is likely some benefit to caching the inverse
## without needing to recompute it each time it is needed.

## This function creates a special "matrix" object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if
## the inverse has already been calculated. If so, it gets the inverse
## from the cache and skips the computation. Otherwise, it calculates
## and returns the inverse of the matrix and sets the value of the
## inverse in the cache via the setinv function.

## Note: This function assumes that the matrix supplied is always
## invertible.

cacheSolve <- function(x, ...) {
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
