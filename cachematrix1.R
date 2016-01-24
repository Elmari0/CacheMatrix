## The function is aimed to take inversed matrix from the cache instead of doing recalculation every time.
## 

## The same logic as in example provided. 4 functions created inside makeCacheMatrix.

makeCacheMatrix <- function(x = matrix()) {
 	m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function() m <<- solve(x)
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve is using functions initiated above for replacement of computation.

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
