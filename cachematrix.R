## R Programming assignment 2
## Creating cache for matrix inversion

## the makeCacheMatrix creates a special matrix which is really a list containing a function to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverted matrix
## get the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
    invertedMatrix <- NULL
    setMatrix <- function(y) {
            x <<- y
            invertedMatrix <<- NULL
    }
    getMatrix <- function() x
    setInvertedMatrix <- function(inverted) invertedMatrix <<- inverted
    getInvertedMatrix <- function() invertedMatrix
    list(setMatrix = setMatrix,
         getMatrix = getMatrix,
         setInvertedMatrix = setInvertedMatrix,
         getInvertedMatrix = getInvertedMatrix)
}


## This function computes the inverse of the special
## "matrix" returned by 'makeCacheMatrix' above. If the inverse has
## already been calculated (and the matrix has not changed), then
## 'cacheSolve' should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    invertedMatrix <- x$getInvertedMatrix()
    if(!is.null(invertedMatrix)) {
        message("Using cached inverted matrix")
        return(invertedMatrix)
    }
    data <- x$getMatrix()
    invertedMatrix <- solve(data, ...)
    x$setInvertedMatrix(invertedMatrix)
    invertedMatrix
}
