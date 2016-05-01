## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## creates a special “matrix” object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
  x <<- y
  inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}



## Write a short comment describing this function
## omputes the inverse of the “matrix” returned by makeCacheMatrix(). 
##If the inverse has already been calculated and the matrix has not changed, it’ll retrieves the inverse from the cache directly.
cacheSolve <- function(x, ...) {

  inv = x$getinv()

  if (!is.null(inv)){
    message("matrix did not change, getting cached data")
    return(inv)
  }
  mat.data = x$get()
  inv = solve(mat.data, ...)
  x$setinv(inv)
  return(inv)
}

