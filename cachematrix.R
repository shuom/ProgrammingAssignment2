## Two functions which creates a special instance of the a "matrix" object that has
## the capability to do caching. The "makeCacheMatrix" function is the instantiator
## and the "cacheSolve" function does the calculation.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  #initial inverse value as NULL
  i <- NULL
  
  #store matrix, clear inverse
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  #return cached matrix
  get <- function() x
  
  #overwrite existing inverse data
  setinverse <- function(inverse) i <<- inverse
  
  #return list of functions
  getinverse <- function() i
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` 
## above. If the inverse has already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.
##
## 'x' must be an object returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  #grab cached inverse if it exists
  i <- x$getinverse()
  
  #if cached inverse exists, return i
  if(!is.null(i)) {
    message("Getting cached inverse")
    return(i)
  }
  
  #grab stored data
  data <- x$get()
  
  #solve for the inverse
  i <- solve(data, ...)
  
  #store inverse
  x$setinverse(i)
  
  # Return a matrix that is the inverse of 'x'  
  i
}
