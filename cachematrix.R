# Matrix inversion is usually a costly computation and there may be some
# benefit to caching the inverse of a matrix rather than computing it repeatedly

# makeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    # we start by setting the inverse to NULL
    m <- NULL
    # Create a funchion to cache the inverse once it's set (<<-)
    set <- function(y) {
        x <<- y
        # reset m to NULL if gets changed
        m <<- NULL
    }
    # function to retrieve the original matrix
    get <- function() x
    # function to set the inverse into the cache (m <<-)
    setinverse <- function(inver) m <<- inver
    # function to get the inverse from the cache (m)
    getinverse <- function() m
    # the functions that can be used with makeCacheMatrix
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
# cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix
# above. If the inverse has already been calculated (and the matrix has not changed),
# then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    # call the getinverse function above and either return a NULL or the inverse
    m <- x$getinverse()
    # Check and see if we've already solved the inverse. We haven't if m == NULL
    # So if m is not NULL, then we have cached it, and all we do is return it.
    if(!is.null(m)) {
        message("getting cached matrix inverse")
        return(m)
    }
    # otherwise it hasn't been cached yet, solve and cache
    # put the original matrix in data
    data <- x$get()
    # "Solve" the matrix (data) for the inverse of 'x'
    m <- solve(data)
    # then cache it (m) for the next time
    x$setinverse(m)
    # Return the matrix that is the inverse of 'x'
    m
}

# to test:
# Create a matrix to use for testing
# mdat<-matrix(c(1,3,3, 1,4,3, 1,3,4),nrow=3,ncol=3,byrow=TRUE)
# solve(mdat)
# http://www.purplemath.com/modules/mtrxinvr.htm

# a<-makeCacheMatrix(mdat)
# a$getinverse()
# cacheSolve(a)
# a$getinverse()
# cacheSolve(a)