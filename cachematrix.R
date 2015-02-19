
# Create a template/blueprint/class for a matrix with four predefined operations(methods).
# Invoking this function given a invertile square matrix will instantiate a "special matrix"(object). 
# This object is encapsulated with functions which support inversion of the matrix and its associated storage.
# 
# The four pre-defined functions are:
# set          - creates new 'special matrix' object and initializes its inverse
# get          - echoes back the matrix 
# setinverse   - takes a given inverse matrix as part of this object
# getinverse   - echoes back the inverse matrix
#
makeCacheMatrix <- function(mx = matrix()) {
  # initialize the inverse, imx
  imx <- NULL
  
  # set object to a new one(new input matrix) without invoking makeCacheMatrix
  set <- function(yx) {
    mx <<- yx
    imx <<- NULL
  }
  get <- function() mx
  
  # use this to set inverse matrix as desired
  setinverse <- function(in_imx) imx <<- in_imx
  getinverse <- function() imx
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
} 

cacheSolve<- function(mx, ...) {
  imx <- mx$getinverse()
  if(!is.null(imx)) {
    message("getting cached data")
    return(imx)
  }
 # data <- mx$get()
  imx <- solve(mx$get())
  mx$setinverse(imx)
  imx
}

