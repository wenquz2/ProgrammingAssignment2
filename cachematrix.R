## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special matrix that could cache its inverse. 
## It contains the matrix as well as its cache information.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) inverse <<- inv
        getInverse <- function() inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## Write a short comment describing this function
## This function solves the inverse of the speical matrix created by the previous function. However, it first checks to see if the inverse has already been calculated.
## If so, it "get"s the Inverse from the cache and skips the computation. Otherwise, it calculated the inverse of the matrix and store it in the cache using the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
        inverse
}

