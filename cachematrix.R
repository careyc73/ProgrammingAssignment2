## makeCacheMatrix and cacheSolve cooperate together to allow for creation of
## a matrix and caching of the inverse (i.e., the result of the base package "solve" function).
## makeCacheMatrix can be used to create the initial matrix, the adjunct function 
## cacheSolve will take a cache returned from makeCacheMatrix and solve it, caching
## the result with the matrix where it can be retrieved by the getinverse call.

## makeCacheMatrix: Creates a list of methods allowing retrieval and assignment of a matrix
## variable, additionally provide methods to set and retrieve the inverse
## of the matrix.  The makeCacheMatrix methods expects an initial matrix passed in.
## The names of the methods in the returned list are:
## set, get, setinverse, getinverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve will take a matrix returned by makeCacheMatrix and return the inverse of the
## matrix.  Additionally it will cache the result for improved performance on subsequent calls.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if (!is.null(i)) {
        message("retrieving cached inverse")
        return (i)
    }
    
    x$setinverse(solve(x$get(), ...))
    x$getinverse()
}
