## These functions together allow caching the inverse of a matrix so that it
## doesn't need to be recomputed.

## The makeCachMatrix function creates a matrix object that is able to compute
## an inverse, taking advantage of the fact that if the inverse has been 
## computed once, it can simply be cached and returned rather than recomputed
makeCacheMatrix <- function(theMatrix = matrix()) {
    # A place to store the matrix inverse
    theInverse <- NULL
    # Save the matrix and remove any cached inverse
    set <- function(x) {
        theMatrix <<- x
        theInverse <<- NULL
    }
    # Return the current contents of the matrix
    getMatrix <- function() theMatrix
    # Save and return the matrix inverse
    setInverse <- function(y) theInverse <<- y
    getInverse <- function() theInverse
    # Return the list of functions for the special matrix object
    list (set = set, 
          getMatrix = getMatrix,
          setInverse = setInverse,
          getInverse = getInverse)
}


## The cacheSolve function looks to see if a matrix inverse has already been
## computed. If it has, the function avoids the computation load and returns 
## the cached inverse. If not, it computes, then saves the matrix inverse.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'. Get it from the cache 
    ## if possible
    # First look in the cache
    xInv <- x$getInverse()
    # If there's an inverse there, then return it
    if (!is.null(xInv)) {
        message('Returning cached inverse')
        return(xInv)
    }
    # Nothing in the cache, so get the matrix and compute its inverse
    xMat <- x$getMatrix()
    xInv <- solve(xMat)
    # Put the inverse in the cache in case we need it
    x$setInverse(xInv)
    # Return the computed inverse
    xInv
}
