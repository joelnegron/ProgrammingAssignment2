## The following function work together to calculate the inverse matrix
## and cache its value. If the value for the matrix was previously cached
## it returns the cache value instead of calculating the inverse value again.

## The below function creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The below function computes then inverse matrix in the "makeCacheMatrix"
## function and cache it's value. If the inverse was previously cache, it
## return the cache value instead of recalculating it.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
