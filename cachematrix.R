
## makeCacheMarix
## get: return the matrix that need to be inversed
## set: set the new matrix that need to be invesed
## getInverse: return inversed matrix if already computed
## setInverse: setter to set inversed matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL

    get <- function() x
    set <- function(y) {
            x <<- y
            inverse <<- NULL
    }
    setInverse <- function(inv) {
        inverse <<- inv
    }
    getInverse <- function() inverse

    list(get=get, getInverse=getInverse, set=set, setInverse=setInverse)
}


## cacheSolve - takes list of functions returned by makeCachedMatix, which are used in this function
##              to return inverse matrix, if already computed returned from cache.

cacheSolve <- function(x) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
}
