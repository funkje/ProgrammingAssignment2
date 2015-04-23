## A set of two functions that allow us to automatically store an inverse of a
## matrix once it has been already solved

## Constructs a list of functions for storing and manipulating
## matrix and its inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Returns cached inverse matrix of a matrix if it has been solved
## otherwise it solves it and stores it in the 'x' object
cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)){
        message("getting the inverse from the cache")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}