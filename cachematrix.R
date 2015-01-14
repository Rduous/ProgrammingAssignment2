## A set of functions to optimize repeated calls for the inverse of a matrix.
## Use these to create "matrices" that can cache their inverse matrix.

## Function that accepts a matrix and returns an augmented matrix object which 
## can cache the value of the matrix's inverse. The getInverse function (a 
## named member of the "matrix") will calculate the inverse the first time the
## function is called, and thereafter return the cached value. Altering the 
## matrix (via the set function) will cause the cache to be flushed.

makeCacheMatrix <- function(x = matrix()) {
    # The cache variable
    i <- NULL
    
    # The getInverse function will lazy-initialize the cache variable
    getInverse <- function(...) {
        if (is.null(i)) {
            i <<- solve(x, ...)
        }
        i
    }
    
    # A getter to allow access to the matrix data
    get <- function() x
    
    # The setter changes the data of the "matrix" and flushes the cache
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    # Return a "matrix" object (really a list with functions)
    list(set = set, get = get, getInverse = getInverse)
}


## Function which accepts an augmented "matrix" object and uses its getInverse
## function to return the inverse of the matrix. 

cacheSolve <- function(x, ...) {
        i <- x$getInverse(...)
        i
}
