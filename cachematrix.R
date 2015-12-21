## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(theMatrix = matrix()) {
    theInverse <- NULL
    set <- function(x) {
        theMatrix <<- x
        theInverse <<- NULL
    }
    getMatrix <- function() theMatrix
    setInverse <- function(y) theInverse <<- y
    getInverse <- function() theInverse
    list (set = set, 
          getMatrix = getMatrix,
          setInverse = setInverse,
          getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    xInv <- x$getInverse()
    if (!is.null(xInv)) {
        return(xInv)
    }
    xMat <- x$getMatrix()
    xInv <- solve(xMat)
    x$setInverse(xInv)
    xInv
}
