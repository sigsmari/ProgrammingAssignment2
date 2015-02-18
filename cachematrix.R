## Creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  set_inv <- function(inv) i <<- inv
  get_inv <- function() i
  list(set = set, get = get,
  set_inv = set_inv,
  get_inv = get_inv)

}


## Computes the inverse of the matrix returned by makeCacheMatrix. 
## If the inverse has already been calculated, then it retrieves the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  
  i <- x$get_inv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$set_inv(i)
i
}
