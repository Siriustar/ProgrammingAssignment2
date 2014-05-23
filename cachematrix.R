## Description of code:
## Getting a matrix value from user-instantiated variable,
## sets it for use outside the current function's environment,
## and solves for the inverse of the given matrix
## (caught from the function makeCacheMatrix).

## makeCacheMatrix:
## Gets a matrix value from user input,
## sets it for use outside the current function's environment
## by passing a list of global variables
## to the below function 'cacheSolve'

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve:
## Getting the inverse of the matrix called into
## the above function 'makeCacheMatrix' by using a list
## of its global variables

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
