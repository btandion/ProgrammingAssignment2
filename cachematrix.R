
# Create a template/blueprint/class for a matrix with four predefined operations(methods).
# Invoking this function given a invertile square matrix will instantiate a "special matrix"(object). 
# This object is encapsulated with functions which support inversion of the matrix
#
# The four pre-defined functions are:
# set          - creates new 'special matrix' object and initializes its inverse
# get          - echoes back the matrix 
# setinverse   - takes a given inverse matrix as part of this object
# getinverse   - echoes back the inverse matrix
#
makeCacheMatrix <- function(mx = matrix()) {
  # Initialize the inverse, imx
  imx <- NULL
  
  # Set object based on a new input matrix without invoking makeCacheMatrix
  # As such, the inverse is init to Null also
  set <- function(yx) {
    mx <<- yx
    imx <<- NULL
  }
  
  # Provide the matrix as requested
  get <- function() mx
  
  # use this to set inverse matrix as desired; usually from result of solve()
  setinverse <- function(in_imx) imx <<- in_imx
  
  # Provide the inverse matrix as requested
  getinverse <- function() imx
  
  # Return list of functions(methods) related to this 'special matrix'(object)
  # This list becomes the main interface between the object and outside world 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
} 

#
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

