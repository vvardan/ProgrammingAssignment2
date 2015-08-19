## Script to cache the inverse of a matrix
##
## The purpose of this script is to create an alternative to repeatedly
## computing matrix inversion in order to speed up the computing process.
##
## This is done using two main functions: makeCacheMatrix and cacheSolve.
##
## makeCacheMatrix will create a special "matrix", in this case a list of
## functions that will do the following:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse matrix
## 4. Get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(
        set = set,
        get = get,
        setinv = setinv,
        getinv = getinv
    )
}

## cacheSolve calculates the inverse of the matrix created by makeCacheMatrix.
## The function will first verify if the matrix inverse has already been
## calculated:
## If the inverse has been calcuated, it will simply acquire the inverse from
## the cache.
## Otherwise, the inverse will be calculated and the inverse value will be set
## in the cache using the setinv function. 
cacheSolve <- function(x, ...) {
       inv <- x$getinv()
       if(!is.null(inv)){
           message("Getting cached inverse matrix")
           return(inv)
       }
       matrix <- x$get()
       inv <- solve(matrix,...)
       x$setinv(inv)
       inv
}