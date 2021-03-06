## Given that matrix inversion is in general a costly computation, we want to cache the inverse of a matrix
## once it is computed for the first time and  subsequently return the cached value instead of computing it
## over and over again. The following functions, working in conjuction, make this possible.
##
## Example of how to use it:
##
## # create your invertible square matrix:
## test_matrix <- matrix(rexp(100), 10, 10)
##
## # create a cached matrix:
## cached_test_matrix <- makeCacheMatrix(test_matrix)
##
## # compute the inverse using caching:
## cacheSolve(cached_test_matrix)

makeCacheMatrix <- function(x = matrix()) {
  ## Creates a special "matrix" object that can cache its inverse.
  ##
  ## Args:
  ## x: An invertible matrix
  ##
  ## Returns:
  ## A special "matrix" object that caches its inverse.
  
  cached_inverse_of_x <- NULL # initializes cache with NULL

  # create set() and get() functions so cacheSolve() can read and update the matrix data
  set <- function(y) {
    x <<- y
    cached_inverse_of_x <<- NULL
  }
  get <- function() x

  # create setinverse() and getinverse() functions so cacheSolve() can read and update the cached matrix inverse
  setinverse <- function(z) cached_inverse_of_x <<- z
  getinverse <- function() cached_inverse_of_x

  # builds and returns the "matrix" object
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  ## Computes the inverse of the  special "matrix" returned by makeCacheMatrix above.
  ## If the inverse has already been calculated (and the matrix has not changed), then cachesolve()
  ## should retrieve the inverse from the cache.
  ##
  ## Args:
  ## x   : An invertible matrix.
  ## ... : Any arguments following the first argument supported by the function solve()
  ##
  ## Returns:
  ## A matrix that is the inverse of 'x'

  # get cached value for inverse of x and return it if it is not NULL
  inverse_of_x <- x$getinverse()
  if(!is.null(inverse_of_x)) {
    return(inverse_of_x)
  }
  
  # If you are here, this means cache is NULL
  data <- x$get()                     # get data from "matrix" object
  inverse_of_x <- solve(data, ...)    # compute the inverse using solve()
  x$setinverse(inverse_of_x)          # update cached value (then don't need to compute it again next time)
  inverse_of_x                        # return inverse
}
