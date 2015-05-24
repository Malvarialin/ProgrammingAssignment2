## These functions calculate and cache the inverse of a given matrix so that it may be used in later calculations.


## makeCacheMatrix creates a special list that stores the values of the matrix,
## and caches the inverse.

makeCacheMatrix <- function(x = matrix()) {
                m <- NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                get <- function() x
                setinverse <- function(solve) m <<- solve
                getinverse <- function() m
                list(set=set, get = get,
                     setinverse=setinverse,
                     getinverse = getinverse)
}


## cacheSolve does the actual calcuation of the inverse, but first it checks to see if
## it is already cached in the makeCacheMatrix function.
## It will skip the calculation if the inverse is found.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message('getting cached data')
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
                
}
