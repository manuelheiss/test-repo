## The following two functions provide the ability to compute the inverse of
## a matrix in a cached way. This can save a lot of computational time.

## Function does the same as makeVector, only use solve instead of mean
## and x is a matrix not a vector.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Check via getinverse if inverse already has been calculated, if so return
## it from cache. Else calculate inverse with solve function, store it with
## setinverse in cache and return it.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setinverse(m)
  m
}