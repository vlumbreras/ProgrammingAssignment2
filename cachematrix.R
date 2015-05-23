## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## Assuption: the matrix is invertible

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {                     ## Set the matrix object
        x <<- y
        inverse <<- NULL
    }
    get <- function() x                      ## Return matrix object
    setinverse <- function(solve) inverse <<- solve  ## Calculate inverse using solve
    getinverse <- function() inverse                 ## Return the inverse
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.
## Assuption: the matrix is invertible

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")  ##Inverse previously calculated
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)         ##Compute inverse for the first time
    x$setinverse(inverse)
    inverse
}