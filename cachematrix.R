  ## Inverse of a Matrix can be cached through this set of functions because compulating Matrix inversion can be time consuming
  ## A storage of a matrix can be created for caches the inverse to avoid computing it repeatedly


## Create a matrix to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The inverse of matrix created by previous function will be calculated by this function
## If the inverse has been calculated for the same matrix, the results will be retrieved from the cache instead of generating a new one

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
