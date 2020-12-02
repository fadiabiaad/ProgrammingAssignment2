## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix is a function that creates a special matrix and cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special matrix created by
## the function makeCacheMatrix . if the inverse has been calculated and the matrix has not changed, 
## then retrieve the data from the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
inv <- x$getinv()
if(!is.null(inv)) {
message("getting cached result")
return(inv)
}
data <- x$get()
inv <- solve(data, ...)
x$setinv(inv)
inv
}

m <- matrix(rnorm(16),4,4)
m1 <- makeCacheMatrix(m)
cacheSolve(m1)
