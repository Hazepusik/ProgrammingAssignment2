## Functions that cache the inverse of a matrix

## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
  
  inv <- NULL  ## Initialize the inverse property
  
  ## Method to set the matrix
  set <- function( matrix ) {
    m <<- matrix
    inv <<- NULL
  }
  
  ## Method the get the matrix
  get <- function() {
    m    ## Return the matrix
  }
  
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  ## Method to get the inverse of the matrix
  getInverse <- function() {
    inv    ## Return the inverse property
  }
  
  ## Return a list of the methods
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Compute the inverse of the special matrix (from makeCacheMatrix)
## If the inverse has already been calculated, return cache value
cacheSolve <- function(x, ...) {
  m <- x$getInverse() ## Return a matrix that is the inverse of 'x'
  
  ## return cache inverse if it already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setInverse(m)
  
  ## Return the matrix
  m
}