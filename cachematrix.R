## README
## We define three functions:
## - makeCacheMatrix: define the matrix object
## - cacheSolve: define the computation and caching
## - cacheTest: a small test function to test 
##
##
## Example usage:
## source("cachematrix.R")
## m <- matrix(c(0, 2, 1, 0), nrow = 2, ncol = 2, byrow = TRUE)
## cacheTest(m)
## [1] TRUE


## makeCacheMatrix:
##  Define a matrix object that can cache its own inverse.
##  The matrix itself is provided by function parameter `x`
## and its inverse is defined by `matinv`. The functions for
## this matrix object is defined by:
## -`set` update the matrix itself. When setting a new value
## the previous cached inverse matrix is invalidated.
## -`get` obtain the value of the matrix  
## -`getinservse` gives the current cached inverse matrix
## -`setinverse` updates the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
  
  # matrix inverse
  matinv <- NULL
  
  # set the value of new matrix
  set <- function(y) {
    x <<- y
    # Now the inverse is invalid. Null it.
    matinv <<- NULL
  }
  
  # get the marix
  get <- function() x
  
  # Update the inverse of the matrix
  setinverse <- function(mi) {
    # mi should be a matrix
    if (!is.matrix(mi)) {
      stop("The input should be a matrix")
    }
    matinv <<- mi
  }
  
  # Get the marix inverse
  getinverse <- function() matinv
  
  # Define the "matrix" object and its functions
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cachesolve:
##  Computes the inverse of a matrix on the "first" computation
##  and caches the result for the consequent usages.
##
## Assumptions from the assignment README:
##  - The matrix provided is always invertible.
##  - We assume to be able to always use `solve` to get the inverse.
cacheSolve <- function(x, ...) {
  # Try to get the inverse possibly from cache
  minv <- x$getinverse()
  if (!is.null(minv)) {
    message("Using cached inverse matrix")
    return(minv)
  }
  
  # The inverse was not cached. 
  # 1. Compute the inverse
  tmp_mat <- x$get()
  minv <- solve(tmp_mat, ...)
  # 2. Cache it
  x$setinverse(minv)
  minv
}


## cacheTest
##  We use the definition of the inverse of a matrix
##  to test the correctness of the above functions
cacheTest <- function(x = matrix(), ...) {
  mat <- makeCacheMatrix(x)
  matinv <- cacheSolve(mat)
  t <- matinv %*% x == diag(nrow(x))
  all(t)
}
