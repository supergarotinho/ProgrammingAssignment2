## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ## variable that caches the inverse
    inv <- NULL
    
    ## Setter for the data
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## Getter for the data
    get <- function() x
    
    ## Setter for the inverse var
    setinverse <- function(inverse) inv <<- inverse
    
    ## Getter for the inverse var
    getinverse <- function() inv
    
    ## Return the named list of functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    
    ## Verify if we have the cached inverse of 'x'
    if(!is.null(inverse)) {
        ## Return the cached inverse of 'x'
        message("getting cached data")
        return(inverse)
    }
    
    ## Calculates the inverse and cache it
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    
    ## Return a matrix that is the inverse of 'x'
    inverse
}
