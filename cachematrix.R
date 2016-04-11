##  Two functions are defined in this file:
##
##  1. makeCacheMatrix is a function "factory"
##  which creates "cacheable" matrix objects.
##
##  2. cacheSolve retrieves the inverse from a 
##  "cacheable" matrix object created through
##  makecacheMatrix. The value of the inverse
##  will come from cache if the matrix has already
##  been computed, else the matrix inverse is
##  calculated using the "solve" function from the
##  base package.
##
##  Usage:
##  > X <- matrix(c(1,3,0,2),2,2)
##  > cm <- makeCacheMatrix()
##  > cm$set(X)
##  > cacheSolve(cm)
##  [,1] [,2]
##  [1,]  1.0  0.0
##  [2,] -1.5  0.5
##  > cacheSolve(cm)
##  getting cached data
##  [,1] [,2]
##  [1,]  1.0  0.0
##  [2,] -1.5  0.5
##  >

## This function creates a special "matrix" object that can cache 
## its inverse.
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

## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        # The inverse has already been cached,
        # return value from cache
        message("getting cached data")
        return(i)
    }
    # 
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}