## Functions to cache the inverse of a matrix.
##
## With these functions, each matrix created will be able to hold its own
## inverse to be fast retrieved without doing the costly computation to
## calculate it each time it is needed.
##
## The matrix must be created with 'makeCacheMatrix' and its inverse must be
## retrieved with 'cacheSolve' 

## Creates a matrix that can hold its inverse.
##
## Arguments: 
##   x: an initial matrix. If not defined, an empty matrix will be created.
##
## Returns: a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL # will hold the inverse of the matrix, i.e., result of solve
        
        set <- function(y) {
                x <<- y
                s <<- NULL # when a new matrix is set, the old inverse must
                           # be erased
        }
        
        get <- function() x
        
        setsolve <- function(solve) s <<- solve
        
        getsolve <- function() s
        
        list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}


## Returns the inverse of the matrix. If it has already been calculated,
## returns the cached information. Otherwise, calculates the inverse of the
## matrix, saves to the cache, and returns it.
##
## Arguments:
##   x: the matrix created with makeCacheMatrix
##   ...: extra arguments for the 'solve' function
##
## Returns: a matrix that is the inverse of 'x'.

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        
        s
}
