## The pair of functions below provide facility for calculating and caching the inverse of a matrix for reuse 
## in order to save processing time

## This function creates a special "matrix" object that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set<-function(y){
        x <<- y
        inv <<- NULL
    }
    get<-function() x
    setInverse<-function(Invrs) inv <<- Invrs
    getInverse<-function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## It first looks into the cache for previously calculated inverse to save processing time.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("getting cached inverse")
        return (inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
}
