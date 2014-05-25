## makeCacheMatrix creates a "special" matrix that contains functions to store and retrieve itself and its inverse. 
## cacheSolve inputs the "special" matrix created in makeCacheMatrix. If the inverse of the matrix is already calculated,
## it is returned from cache. Else, it is calculated, stored in the cache and returned.

## makeCacheMatrix creates a "special" matrix that returns functions to store and retrieve itself and its inverse. 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## cacheSolve returns the inverse of a matrix from its cache. It calculates the inverse of a matrix,
## if null and stores in the cache before returning. 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
          message("Getting cached inverse")
          return(i)
        }
        
        i <- solve(x$get())
        x$setinverse(i)
        i
}
