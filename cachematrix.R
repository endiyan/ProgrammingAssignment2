## The makeCacheMatrix accepts a matrix as an argument
## The makeCacheMatrix function returns a list. The elements contains 
## the get and set methods for a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        ## new matrix means new inverse value, therefore delete the cache
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(i) inverse <<- i
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The cacheSolve function accepts a list created by the makeCacheMatrix and
## other parameters as arguments
## This functions caches and returns the value of the inverse of a matrix

cacheSolve <- function(x, ...) {
    
    ## if available, return the cached inverse value
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    ## calculate the inverse then caches it
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}

