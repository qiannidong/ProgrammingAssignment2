## These pair of functions can cashe the inverse of a matrix.
## Becaue matrix inversion can be time consuming, caching the inverse of a matrix could be benefitial.
## These two functions will generate a storage for matrix and caching its inverse.

## Generate a matrix to cache its inverse.

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


## To calculate the inverse of matrix generated from previous function. 
## The inverse will be tritrieved from the cache if it has been calculated.

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
