## The makeCacheMatrix function creates a (square invertible) matrix object that can cache its inverse. The cacheSolve
## function calculates the inverse of a matrix object created with the previous function

## Creates a (square invertible) matrix as a list of functions to get and set the values of the matrix and the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ## the inverse of the matrix
  ## set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## get the value of the matrix
  get <- function() x
  ## set the value of the inverse
  setInv <- function(i) inv <<- i
  ## get the value of the inverse
  getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Calculates the inverse of a matrix object (created with makeCacheMatrix function): 
## if the inverse has already been calculated, retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)) return(inv)
  m <- x$get()
  inv <- solve(m, ...)
  x$setInv(inv)
  inv
}
