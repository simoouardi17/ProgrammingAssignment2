makeCacheMatrix <- function(x = matrix()) {
  # Create an environment to store the matrix and its inverse
  env <- new.env()
  env$matrix <- x
  env$inverse <- NULL
  
  # Method to set the matrix
  set <- function(y) {
    env$matrix <- y
    env$inverse <- NULL
  }
  
  # Method to get the matrix
  get <- function() env$matrix
  
  # Method to set the inverse of the matrix
  setinverse <- function(inverse) env$inverse <- inverse
  
  # Method to get the inverse of the matrix
  getinverse <- function() env$inverse
  
  # Output list with closures referencing the environment
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
cacheSolve <- function(x, ...) {
  # Try to retrieve the cached inverse matrix
  if (!is.null(m <- x$getinverse())) {
    message("getting cached data")
    return(m)
  }
  
  # Otherwise, calculate the inverse, cache it, and return it
  m <- solve(x$get(), ...)
  x$setinverse(m)
  m
}
