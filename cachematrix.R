## These two functions work in conjunction to cache potentially time-consuming
## computations to allow for more efficient computer utilization. By using scoping
## rules of the R language, these functions can preserve state inside of an
## R object.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix. If the inverse has already been calculated and unmodified,
## then the cacheSolve function will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("Retrieving cached inverse value")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
