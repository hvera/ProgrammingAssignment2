#########################################################################################
## makeCacheMatrix will create a type of matrix that can hold its inverse in memory
## to avoid recalculating the inverse every time it's needed.
## The CacheMatrix works in conjunction with the cacheSolve function to initialize
## the cached data when the inverse is first requested.
## setinverse is used to remember (memoize) the cached value.
## getinverse returns the inverted matrix, or NULL if it hasn't been initialized.
## get returns the original matrix to be inverted.

makeCacheMatrix <- function(x = matrix()) {
    inverted <- NULL
    set <- function(y) {
        x <<- y
        inverted <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inverse) inverted <<- inverse
    getinverse <- function() inverted
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

#########################################################################################
## cacheSolve calculates the inverse of the special matrix created with
## makeCacheMatrix. It also caches the result for speed when the inverse is 
## needed again.
##
## The first time it is called on this matrix, the solve() function is used
## to generate the inverse. It is then chached internally and returned.
## All other times this function is called, the cached version is returned without
## invoking the (potentially expensive) solve() function.
##

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverted <- x$getinverse()
    if(is.null(inverted)) {
        data <- x$get()
        inverted <- solve(data, ...)
        x$setinverse(inverted)
    } else {
        message("returning cached version...")
    }
    inverted
}
