## Creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the cached data to NULL
  cache <- NULL

  # Setter function to set the matrix
  set <- function(y) {
    x <<- y
    cache <<- NULL # Invalidate the cache when the matrix is set
  }

  # Getter function to get the matrix
  get <- function() x

  # Function to compute and cache the inverse of the matrix
  cacheSolve <- function() {
    if (!is.null(cache)) {
      message("Getting cached data")
      return(cache)
    }
    message("Computing the inverse")
    inv <- solve(x)
    cache <<- inv
    inv
  }

  # Return a list of functions
  list(set = set, get = get, cacheSolve = cacheSolve)
}

## Computes the inverse of a matrix, caching it if possible.
cacheSolve <- function(x, ...) {
  if (!is.null(x$cache)) {
    message("Getting cached data")
    return(x$cache)
  }
  message("Computing the inverse ")
  inv <- solve(x$get(), ...)
  x$cache <- inv
  inv
}
