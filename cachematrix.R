## Matrix inversion can be a costly computation and there may be some benefit to
## caching the inverse of the matrix rather than compute it repeatedly. The 
## following pair of functions cache the inverse of a matrix.  

## The makeCacheMatrix function creates a special matrix object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
    InvMatrix <- NULL
    set <- function(y) {
        x <<- y
        InvMatrix <<- NULL
    }
    get <- function() x
    setInverse <- function(Inverse) InvMatrix <<- Inverse
    getInverse <- function() InvMatrix
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}


## The cacheSolve function computes the inverse of the special matrix returned 
## by the makeCacheMatrix function. If the inverse has already been computed then
## the cacheSolve function should retrieve the inversed matrix from the cache.  

cacheSolve <- function(x, ...) {
    ## Returns a matrix that is the inverse of 'x'
    InvMatrix <- x$getInverse()
    
    if(!is.null(InvMatrix)) {
        message("getting cached data")
        return(InvMatrix)
    }
    imatrix <-x$get()
    InvMatrix <- solve(imatrix, ...)
    x$setInverse(InvMatrix)
    InvMatrix
}

