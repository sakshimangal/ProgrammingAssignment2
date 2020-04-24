##  a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  matInverse <- NULL
  set <- function(y) {
    x <<- y
    matInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) matInverse <<- inv
  getInverse <- function() matInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##  This function computes the inverse of the special "matrix" returned 
##  by makeCacheMatrix above. If the inverse has already been calculated 
##  (and the matrix has not changed), then the cachesolve should retrieve 
##  the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matInverse <- x$getInverse()
  if(!is.null(matInverse)) {
    message("getting cached data")
    return(matInverse)
  }
  data <- x$get()
  matInverse <- solve(data) %*% data
  x$setInverse(matInverse)
  matInverse
}
