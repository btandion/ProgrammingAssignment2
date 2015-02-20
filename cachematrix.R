#########################################################################################
# Create a template/blueprint/class for a matrix with four predefined operations(methods).
# Invoking this function given a invertible square matrix will instantiate a "special matrix"(object). 
# This object is encapsulated with functions which support inversion of the matrix
#
# The four pre-defined functions are:
# set          - creates new 'special matrix' object and initializes its inverse
# get          - echoes back the matrix 
# setinverse   - takes a given inverse matrix as part of this object
# getinverse   - echoes back the inverse matrix
#
# This function together with cacheSolve() provides a way to instantiate a matrix object 
# and its inverse. The inverse can be cached in the object for fast access.
#
makeCacheMatrix <- function(mx = matrix()) {
  # there is no inverse matrix at the beginning
  imx <- NULL
  
  # Set object based on a new input matrix without invoking makeCacheMatrix
  # As such, the inverse is also init to Null
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

#########################################################################################
# Once the 'special matrix' object is created(instantiated), cacheSolve can be used to get 
# the inverse. If the inverse already exists(cached), it will be used. Otherwise, the inverse 
# is obtained by invoking solve() and the result is cached for future use.
#
cacheSolve<- function(mx, ...) {
  
  # request the inverse from cache
  imx <- mx$getinverse()
  
  # if it exists, return the inverse and note the origin
  if(!is.null(imx)) {
    message("getting cached data")
    return(imx)
  }
  
  # if it doesnt exist, request the matrix and use solve() to find its inverse.
  # Take the inverse and put it in cache then return the inverse
  getmxdata <- mx$get()
  imx <- solve(getmxdata)
  
  # put the inverse in cache
  mx$setinverse(imx)
  imx
}

