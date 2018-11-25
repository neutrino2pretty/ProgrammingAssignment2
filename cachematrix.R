## Let's write R functions that will be able to cache potentially time-consuming computations.

## The first function: makeCacheMatrix is a function that will create a *matrix* object. This matrixwill be able to 
## cache its inverse for the input

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(z) {
                x <<- z
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## This cacheSolve function is a function that computes the inverse of the *matrix* by makeCacheMatrix
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
