## 03Mar17 - ABR - 
## Create a specialized matrix object that contains:
##   1. Helper attributes include get/set
##   2. An inverse attribute that is the inverse of a square, invertible matrix
## Create a helper function to perform the inverse computation (via solve(x))
## and stores the inverse on the object matrix (i.e. cache) for faster retrieval

## Create a special matrix that knows it's inverse
makeCacheMatrix <- function(x=matrix()){
  ##when we instantiate a new object with a null inverse
  inv <- NULL
  
  ##helper construct to update the matrix values and 
  ##reset the matrix inverse to null
  set <- function(y) {
    x <<- y ##set a new matrix 'x'
    inv <<- NULL ##clear cached inverse 
  }
  
  ##function call to print the results of the current matrix
  get <- function() x
  
  ##override the calculation of the 
  ##inverse by passing in a matrix and 
  ##setting that inverse matrix on the 'inv' attribute of
  ##matrix 'x'; this can be a spoof because it assumes that
  ##the object (overrideInv) actually IS the inverse of the 
  ##the base matrix 'x'
  setinv <- function(overrideInv) inv <<- overrideInv
  
  ##function call to get the inverse 
  getinv <- function() inv
  
  ##create list of attributes that are
  ##exposed on the customized matrix object
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}

cacheSolve <- function(x, ...) {
  ##call function to get inverse from object 'x'
  inv <- x$getinv()
  
  ##inv returned from object 'x' exists
  ##so we do not need to perform the calculation
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ##get the actual matrix associated with object 'x' 
  data <- x$get()
  
  ##perform calc to find the inverse of the matrix
  inv <- solve(data, ...)
  
  ##store the inverse on the object 'x'
  x$setinv(inv)
  
  inv ##print out the matrix inverse
}