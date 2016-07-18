## The following code contains two functions for creating a special "matrix object" to store a matrix and cach its inverse.
## Caching the inverse of the matrix saves time and computational resources instead of repeatidely computing it for the same matrix.

## The first function "makeCacheMatrix" returns the matrix object that can cache the inverse of the given matrix

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) inv <<- inverse
      getInverse <- function() inv
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## The following function computes the inverse of the matrix returned by the previous function "makeCacheMatrix". If the inverse has already been calculated for the same matrix, then the function will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      inv <- x$getInverse()
      if(!is.null(inv)) {
            message("Retrieving Cached Inverse")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setInverse(inv)
      inv
}
