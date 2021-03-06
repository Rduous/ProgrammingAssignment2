## A set of functions to optimize repeated calls for the inverse of a matrix.
## Use these to create "matrices" that can cache their inverse matrix.

## Function that accepts a matrix and returns an augmented matrix object which 
## can cache the value of the matrix's inverse. Altering the 
## matrix (via the set function) will cause the cache to be flushed.
makeCacheMatrix <- function(x = matrix()) {

    # The cache variable
    i <- NULL
    
    # Get the inverse
    getInverse <- function() i
    
    # Set the inverse 
    setInverse <- function(inverse) i <<- inverse
    
    # A getter to allow access to the matrix data
    get <- function() x
    
    # The setter changes the data of the "matrix" and flushes the cache.
    # ASSUMED: the matrix will only be changed via this set method.
    # Malicious users would be able to bypass this; I consider it beyond 
    # the scope of this assignment to defend against it.
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    # Return a "matrix" object (really a list with functions)
    list(set = set,get = get,getInverse = getInverse,setInverse = setInverse)
}


## Function which accepts an augmented "matrix" object and uses its getInverse
## function to return the inverse of the matrix. Utilizes the caching functions
## of the cachematrix to avoid repeated computations of the inverse.
cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if (is.null(i)) {
        print("Calculating inverse")
        i <- solve(x$get(), ...)
        x$setInverse(i)
    }
    return(i)
}
