makeCacheMatrix <- function(x = matrix()) {
  ## x is the a square invertible matrix
  ## return will be a list containing the following functions to
  ## set and get the matrix before setting and getting the inverse for input to cacheSolve()
  
  inv = NULL
  set = function(y) {
    x <<- y
    # assigning a value to x in an environment different from the current environment
    inv <<- NULL
    # assigning a NULL to variable inv in an environment different from the current environment
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
  ## assigning to makeCacheMatrix()
  ## return the inverse of the original matrix input to makeCacheMatrix()
  
  inv = x$getinv()
  
  
  if (!is.null(inv)){
    # if the value of inv is not NULL, draw the data directly get it from the cache and skip computation. 
    message("cached data has been retrieved")
    return(inv)
  }
  
  # if the value of inv is NULL, the following computation occurs.
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # sets the value of the inverse via the function setinv
  x$setinv(inv)
  
  return(inv)
}
