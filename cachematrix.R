## function to create a matrix and cache the inverse 
makeCacheMatrix <- function(x = matrix()) {
  ## empty vector for inverse 
  m <- NULL
  #produce the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # retrieve the matrix
  get <- function() x
  # set the inverse of the matrix 
  ## solve function provides the solution for the inverse of the cached matrix 
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  # function list
  list(set = set, get = get, setinverse=setinverse, getinverse = getinverse)
}
# Computes, caches, and returns    matrix inverse
cacheSolve <- function(x, ...) {
  ##return inverse of matrix
  m <- x$getinverse()
  ## or retrieve the inverse from the cache. 
  if(!is.null(m)) {
    ## message to show whether the inverse has been determined or was cached
    message("getting cached data")
    return(m)
  }
  # retrieve cached matrix 
  matrix <- x$get()
  #solve matrix inverse
  m <- solve(matrix, ...)
  ## set the inverse of the matrix to the vector m 
  x$setinverse(m)
  #  return matrix
  m
}