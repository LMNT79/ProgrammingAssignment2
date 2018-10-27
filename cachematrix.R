## This is the work completed for assignment 2
## The objective of this work is to cache the inverse of a matrix

## the makeCacheMatrix creates a special matrix object which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## the CacheSolve function computes the inverse of the matrix returned by makeCacheMatrix.
## if the inverse has already been calculated and the matrix is unchanged, CacheSolve does a retrieve.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    return(inv)

## Return a matrix that is the inverse of 'x'
}
