## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##This function creates a special matrix and stores it in the cache
makeCacheMatrix <- function(x = matrix()) {
    
    inverse <- NULL
    
    ##Store a new matrix in the cache and reset the inverse
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    
    ##Get the matrix from the cache
    get <- function() x
    
    ##Set the inverse matrix in the cache
    setinverse <- function(inv){
        inverse <<- inv
    }
    
    ##Get the inverse from the cache
    getinverse <- function() inverse
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

## cachesolve returns the inverse of the matrix X stored in the cache before
## or compute the inverse if the matrix has changed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse();
    
    if(!is.null(inv)){
        return(inv)
    }
    
    m <- x$get()
    
    inv <- solve(m)
    
    x$setinverse(inv)
    
    inv
}
