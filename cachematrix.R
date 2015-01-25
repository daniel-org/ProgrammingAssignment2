
## makeCacheMatrix is a function use to create a special matrix assume to be always have an inverse matrix
## makeCacheMatrix has the following 4 nested functions:
## set: set the value of the matrix
## get: get the value of the matrix
## setinverse: calculate the value of the inverse matrix
## getinverse: get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse_matrix) m <<-inverse_matrix
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}



## cacheSolve function calculates the inverse matrix of the matrix created by makeCacheMatrix
## It first check to se if the inverse matrix has already been calculated. If so it gets the 
## inverser matrix from the cache and skips teh computation. Otherwise it calculates the inverse
## matrix and sets the inverse matrix value in the cache via setinverse functiom.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
