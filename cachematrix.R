## Put comments here that give an overall description of what your
## functions do

##This function is used to cache a matrix and its inverse

makeCacheMatrix <- function(data = matrix()) {
  inv <-NULL

  setData <- function(y) {
    data <<- y
    inv <<- NULL
  }
  
  getData <- function() {
    data
  }
  
  setInverse <- function(value) {
    inv <<- value
  }
  
  getInverse <- function () {
    inv
  }
  
  list(set=setData, getData=getData, setInverse=setInverse,getInverse=getInverse)

}


## This function takes in a makeCacheMatrix object and computes its inverse. 
## if the inverse is already available in the cache, the cached value is returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse();
  if (!is.null(inverse)) {
    message(" Returning cached value")
    return (inverse)
  } 
  data <- x$getData();
  inverse <- solve(data)
  x$setInverse(inverse)
  inverse
}