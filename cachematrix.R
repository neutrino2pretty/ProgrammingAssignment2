##===============================================================================================================
## This function creates a "matrix" object that can cache its inverse. Some functions are very time-consuming: 
## taking the mean of a numeric vector is typically a fast operation. However, for a very long vector, it may 
## take too long to compute the mean, especially if it has to be computed repeatedly (e.g. in a loop). If the 
## contents of a vector are not changing, it may make sense to cache the value of the mean so that when we need 
## it again, it can be looked up in the cache rather than recomputed. 
## Once this function is called the he first time or if the set function "set" is called it'll set the inverse 
## to NULL. Hence, if the initial matrix changes in anyway the inverse will be set to NULL. This should satisfy
## the condition "If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache."


makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        
        set <- function(y) {
                if(nrow(y) != ncol(y)) {
                        message("Number of rows != number of columns. The solve function will not work properly.")
                }
                x <<- y
                inverse <<- NULL
        }

        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## ------------------------------------------------------------------------------------------------------------------------------
## This function computes the inverse of the matrix returned by  makeCacheMatrix. If the inverse has already 
## been calculated, then the cachesolve should retrieve the inverse from the cache.
## We know that the matrix hasn't been changed because the inverse will be set to NULL if it does.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        
        if(!is.null(inverse)) {
                message("Getting cached data")
                return(inverse)
        }
        
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}