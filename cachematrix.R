## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
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
        ## Return a matrix that is the inverse of 'x'
        m <- x$get_inverse()
        
        
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
