## Functions for creating matrix wrappper, which stores, besides matrix object 
## itself, also inverse matrix upon inverse matrix computation and returns this 
## cached value when requested next time.


## Creates matrix wrapper - list of functions for storing and getting matrix 
## object itself and inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}


## Computes matrix inverse for matrix wrapper and stores the results in 
## the cache. If there's already inverse matrix computed in the cache, 
## return it instread computing it again.
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("returning cached inverse matrix")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix, ...)
    message("inverse matrix computed, storing result into the cache")
    x$setInverse(inv)
    inv
}

