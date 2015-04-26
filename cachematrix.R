# makeCacheMatrix is a set of functions that compute the inverse of a matrix 
# and store it such that it can be accessed later via cachesolve

makeCacheMatrix <- function(x = matrix()) {
     inv_matrix <- NULL
     set <- function(y) {
          x <<- y
          inv_matrix <<- NULL
     }
     get <- function() x
     setinverse <- function(solve) inv_matrix <<- solve
     getinverse <- function() inv_matrix
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

#cacheSolve checks the cache for an inverted matrix. 
#If none is available, cacheSolve computes and returns the inverted matrix.

cacheSolve <- function(x, ...) {
      inv_matrix <- x$getmatrix
      if(!is.null(inv_matrix)) {
           message("getting cached data")
           return(inv_matrix)
      }
     data <- x$get()
     inv_matrix <- solve(data, ...)
     x$setinverse(inv_matrix)
     return(inv_matrix)
}
