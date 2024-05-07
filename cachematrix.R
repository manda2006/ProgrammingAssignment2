## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse <- 0
  ## set the matrix
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  ## get the matrix
  get <- function(){x}
  
  setInverse <- function(inv) {inverse <<- inv}
  
  ## Get the inverse of the matrix
  getInverse <- function() inverse
  
  ## Return a list of functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## Calculate the inverse
  data <- x$get()
  inv <- solve(data, ...)
  
  ## Cache the inverse
  x$setInverse(inv)
  
  ## Return the inverse
  inv

}
