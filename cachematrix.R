## This R script is designed to to enable the inverse of a square n*n matrix to be cached
## By doing so calculating the inverse on multiple occasions can be avoided potentially speeding up further calculations

## The function makeCacheMatrix creates a framework to store the input matrix x and it's inverse
## the function is able to alter the cache when the inverse of x is first calculated or if is redefined

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

## The function cacheSolve first checks whether the inverse of x has already been cached
## If not the function calculates the inverse and returns it to the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}