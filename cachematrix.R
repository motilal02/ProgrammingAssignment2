## -- Assignment: Caching the Inverse of a Matrix
## -- For easy reading, I set the tab 8

## -- Following is a pair of functions that cache the inverse of a matrix

## -- FUNCTION: "makeCacheMatrix" creates a special matrix object that can cache its inverse.
## -- makeCacheMatrix creates a special "matrix", which is really a list containing a function to:
## -- Set the value of the vector
## -- get the value of the vector
## -- set the value of the inverse
## -- get the value of the Inverse
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL 
        set <- function(y) { 
                x <<- y; 
                inverse <<- NULL; 
        } 
        get <- function() return(x); 
        setinv <- function(inv) inverse <<- inv; 
        getinv <- function() return(inverse); 
        return(list(set = set, get = get, setinv = setinv, getinv = getinv)) 
} 

## -- FUNCTION: "cacheSolve" computes the inverse of the special matrix returned by makeCacheMatrix above. 
## -- If the inverse has already been calculated (and the matrix has not changed), 
## -- then the cachesolve should retrieve the inverse from the cache.
## -- It returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) { 
        inverse <- x$getinv() 
        if(!is.null(inverse)) { 
                message("Getting cached data...") 
                return(inverse) 
        } 
        data <- x$get() 
        invserse <- solve(data, ...) 
        x$setinv(inverse) 
        return(inverse) 
}
