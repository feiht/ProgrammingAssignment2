## Following functions cache the time consuming calculation of
## matrix inversion.

## makeCacheMatrix creates a list for functions to
## set and get values for matrix, set and get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
  
}


## cacheSolve inverses the matrix, if the matrix is cached by the previous
## function then that result is returned.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  ## chekc if already cached
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  
  # calculated not cached 
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
  
}