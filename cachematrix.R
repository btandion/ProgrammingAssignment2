makeCacheMatrix <- function(mx = matrix()) {
  imx <- NULL
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

