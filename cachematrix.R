## The following functions implement a wrapper for a matrix that will cache the 
## invert operation on the matrix. 

## makeCacheMatrix wraps a matrix with functions supporting cached access
## to the inverted form of the matrix.
## get/set will get or set the value of the wrapped matrix, invalidating the cache.
## get/setinverse will get or set the value of the cached inverted matrix.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    getinverse <- function() inverse
    setinverse <- function(i) inverse <<- i
    get <- function() x
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }    
    list(getinverse = getinverse,
         setinverse = setinverse,
         get = get, set = set)
}

## cacheSolve takes a wrapped matrix generated using makeCacheMatrix above and
## either computes, caches and returns the inverted form, if necessary, or
## simply returns the cached inverted form when available.
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("retrieving cached inverted matrix")
        return(i)
    }
    i <- solve(x$get(), ...)
    x$setinverse(i)
    i
}
