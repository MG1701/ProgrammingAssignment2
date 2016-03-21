## These functions calculate and cache the inverse of a given square invertible 
## matrix. If the inverse already exists in a call, it is not re-calculated but
## retrieved from the cache.

## Stores a list of functions to set and retrieve the matrix 'x', 
## as well as set an retrieve its inverse. No error handling included, i.e.
## the matrix is assumed to be a square invertible matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Calculates, caches and returns the inverse of a square invertible matrix 'x'

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("Inverse already calculated: getting cached data")
        return(inv)
    }
    matr <- x$get()
    inv <- solve(matr, ...)
    x$setinverse(inv)
    inv
}
