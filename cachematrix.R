## This is a submission for the second programming assignment for rprog-003
## These functions exploit scoping rules to preserve object states
## These functions are modified from example code provided in the assignment

## This function creates a list of functions which create and return a matrix
## and to create and return the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function () x
        setinv <- function(mtrxinvrs) inv <<- mtrxinvrs
        getinv <- function() inv
        list(set = set,get = get, setinv = setinv, getinv = getinv)
}


## This function actually solves the matrix inverse, but first checks the 
## matrix list from makeCacheMatrix to see if the value has been solved already

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv
}