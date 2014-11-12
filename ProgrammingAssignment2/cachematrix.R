##The makeCacheMatrix function, creates a special "matrix", which contains a list to
##set the value of the matrix
##get the value of the matrix
##set the value of its inverse
##get the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(mm) inv <<- mm
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## cacheSolve function computes the inverse of the special "matrix" returned by
## makeCacheMatrix function.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve returns the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    determinant <- det(data)
    if(0==determinant) {
        inv = NA
        message("matrix determinant is zero, unable to calculate inverse.")
    } else {
        inv <- solve(data, ...)
    }
    x$setInverse(inv)
    inv
}