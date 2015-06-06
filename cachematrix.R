## Create a matrix which can cache its inverse, and a function which will
## obtain the inverse of such a matrix and cache it.
## It is assumed that the matrix supplied is invertible.

## Create a matrix which can store its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setinverse <- function(inverse) {
        inv <<- inverse
    }
    
    getinverse <- function() {
        inv
    }
    
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}

## Return the cached inverse of a CacheMatrix if it has already been calculated, 
## otherwise calculate, cache and return it.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    
    if (is.null(inv)) {
        message("Calculating the inverse.")
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setinverse(inv) 
    }
    
    inv
}
