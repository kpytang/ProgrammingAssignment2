## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# special "vector", that's really a list containing a function to:
# set the value of the vector
# get the value of the vector
# set the value of the mean
# get the value of the mean
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(data) {
    x <<- data
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
