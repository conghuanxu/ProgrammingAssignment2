## Put comments here that give an overall description of what your
## functions do
## This function have two goals, one is give the inverse of a matrix, 
## the other goal is if we have computed the inverse once, and if we 
## need compute the inverse again we can read from the cache

## Write a short comment describing this function    
## This function is to make the cache of the inverse of the matrix.
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


## Write a short comment describing this function
## if the cached data is null, we solve the inverse and make cache
## otherwise we read data from the cached data.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
