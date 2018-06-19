## Week 3 assignment for R Programming in Coursera that will:
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(solveMatrix) inv <<- solveMatrix
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
## cacheSolve: This function computes the inverse of the special "matrix" returned by funtion makeCacheMatrix

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("Getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv      
}
