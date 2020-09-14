## MakeCacheMatrix creates a special matrix allowing SetInverse to call the inverse of a matrix from Cache or create it.


#sets the values of a matrix, gets the #value, sets the values of the inverse of #the matrix, gets the values.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse)m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
# computes inverse of matrix or returns it if cached
cachemean <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}

