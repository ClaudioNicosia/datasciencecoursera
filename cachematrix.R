## The following set of functions use the <<- operator which assigh a value to an 
## Object in an environment that is different from the current one and
## create a special object thst stores a numeric matrix and cache's its inverse.


## Write a short comment describing this function
# The first function, makeVector creates a special "vector", which is 
# really a list containing a function to

# set the matrix vector 
# get the matrix vector
# set the inverse of the Matrix
# get the inverse of the Matrix

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## ------------------------------------------------------------------------
# The following function returns the inverse of the special "vector" 
# created with the above function. However, it first checks to see if the 
# inverse has already been produced. If so, it gets the resuts from the 
# cache and skips the computation. Otherwise, it calculates the inverse of 
# the matrix vector and sets the inverse matrix in the cache via the setinv 
# function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}