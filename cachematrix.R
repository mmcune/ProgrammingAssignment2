## R Programming - Assignment 2
## Create a matrix object and a function that 
## calculates the inverse of the matrix.  The inverse 
## should be cached so that is does not need to be
## recalculated unless the original matrix changes.

## makeCacheMatrix function creates the matrix object

makeCacheMatrix <- function(x = matrix()) {
    # m - holds the inverse matrix, once it has been calculated
    m <- NULL
    # set function - updates the matrix value and 
    # clears the cache containing the inverse
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # get function - returns the matrix
    get <- function() x
    # setinverse - calculates the inverse
    setinverse <- function(solve) m <<- solve
    # getinverse - returns the inverse
    getinverse <- function() m
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
}


## cacheSolve calculates the inverse of 
## the matrix using using the R function solve.
## The result is cached in the matrix object

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    
    # report that cached value is being used
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # no cahced value, so calculate the inverse 
    # and store it in the cache
    m <- solve(x$get())
    x$setinverse(m)
    m
}
