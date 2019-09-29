## The functions below take any matrix and computes it's inverse. These two functions use the cache in order to reduce processing.    

A <- matrix(c(4,3,3,2), 2, 2)
A_in <- makeCacheMatrix(A)
cacheSolve(A_in)

## makeCacheMatrix will compute the inverse on your matrix and cache it for future use.
## This takes a matrix as an object and returns a list of functions specified below:

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get, 
       setsolve = setsolve,
       getsolve = getsolve)
}

## The second function 'cacheSolve' also computes the inverse, however, if the inverse is already present in the cache it will pull that instead, saving processing. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}




