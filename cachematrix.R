## Assignment: Caching the Inverse of a Matrix Matrix inversion is usually a
## costly computation and their may be some benefit to caching the inverse of a
## matrix rather than compute it repeatedly. 

## Your assignment is to write a pair of functions that cache the inverse of a matrix.


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        # Return a list of functions that 
        # set the value of the matrix
        # get the value of the matrix
        # set the inverse of the matrix
        # get the inverse of the matrix
        
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        set_inverse <- function(solve) m <<- solve
        get_inverse <- function() m
        
        list(set = set,
             get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
        
        
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        m <- x$get_inverse()
        
        # Is the data cached ?
        if(!is.null(m)){
                message("Data is cached: Retrieving")
                return(m)
        }
        
        #else compute the inverse
        tempMatrix.data <- x$get()
        m <- solve(tempMatrix.data, ...)
        x$set_inverse(m)
        
        return(m)
        
}
