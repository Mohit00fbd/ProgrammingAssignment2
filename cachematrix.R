## Here, we learn using the <<- operator which is used to 
## assign value to a different environment from current
## environment. The following two functions are used to
## compute and cache the inverse of matrices.

## makeCacheMatrix creates a list containing a function
## to set the matrix, get the matrix, set the inverse, and
## get the inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set=set, get=get, setinverse = setinverse,
         getinverse=getinverse)
}


## The following function calculates inverse of the special
## matrix created with makeCacheMatrix function.
cacheSolve <- function(x, ...) {
    inverse <- x$getinverse
    if(!is.null(inverse)){
        message("getting cached data")
        return(inverse)
    }
    data <- x$get
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
