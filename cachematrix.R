## This file contains a pair of functions that cache the inverse of a matrix.
## Caching the inverse of a matrix can reduce computation time for situations
## where the matrix inverse is being computed repeatedly.

## The makeCacheMatrix function creates a special "matrix" object that can cache
## its inverse.  The special "matrix" object is a list containing functions to
## manipulate the original matrix and/or its inverse.  There is a pair of
## functions to set/get the value of the original matrix and a pair of functions
## to set/get the value of the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      
      ## The set function initializes the cache variable x with the argument y
      ## and it resets (initializes) the cache variable inv (the inverse of
      ## x) to NULL to indicate that it has not been calculated, yet.
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      
      get <- function() x  ## retrieves the matrix from the cache
      
      ## sets the cache variable for the inverse of the matrix to the argument
      ## being passed to the function (inverse)
      setinverse <- function(inverse) inv <<- inverse
      
      getinverse <- function() inv  ## retrieves the inverse from the cache
      
      ## output of the function is a list of functions to manipulate the matrix
      ## and its inverse
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## The cacheSolve function computes the inverse of the special "matrix" returned
## by the makeCacheMatrix function listed above.  If the inverse has already
## been calculated (and the matrix has not changed), then the cacheSove function
## should retrieve the inverse the inverse from the cache.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      
      inv <- x$getinverse()  ## retrieve matrix inverse from the cache
      
      ## Check to see if the inverse is NULL indicating that it has not been
      ## calculated yet.  This also indicates that the matrix has not changed
      ## since everytime the value of the matrix is changed (via the set
      ## function) the the inverse is reset to NULL.  If the inverse is not NULL
      ## (thus,it has already been calculated), then return the inverse value
      ## from the cache.  Otherwise, calculate the inverse, update the cache
      ## of the inverse and return the value of the inverse
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)  ## returns the value of the inverse from the cache
      }
      
      data <- x$get()  ## retrive the matrix
      
      inv <- solve(data, ...)  ## calculate the inverse of the matrix
      
      x$setinverse(inv)  ## update the cache value of the inverse
      
      inv  ## return the value of the newly calculated inverse
}
