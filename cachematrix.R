## There are two functions that are used to created a matrix and cache its 
## inverse. And can calculate the inverse of the matrix.


## This function creates a object "matrix" can cache its inverse, and return a 
## list contains four functions to set/get matrix and its inverse.  

makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix <- NULL
        set <- function(y) {
              x <<- y
              inverseMatrix <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inverseMatrix <<- inverse
        getInverse <- function() inverseMatrix
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the "matrix" that created by the 
## the function makeCacheMatrix. It will return the inverse from the cache if
## the inverse has already been calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inverseMatrix <- x$getInverse()
      if(!is.null(inverseMatrix)) {
            message("getting cached data")
            return(inverseMatrix)
      }
      Matrix <- x$get()
      inverseMatrix <- solve(Matrix)
      x$setInverse(inverseMatrix)
      inverseMatrix
}
