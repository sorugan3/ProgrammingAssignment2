

## The below function creates a matric and calls a function

makematrix <- function(x = matrix()) {
  inv <- NULL           #inv initially has a null value
  set <- function(y) {
    x <<- y
    inv <<- NULL         #null value in local environment
  }
  get <- function() x 
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



# The below function solves in the inverse if it is already not present in the cache and returns the inverse
  cachinv <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")      #displays this message if the inv is already present
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)       #solves this if the inverse is not present
  x$setinv(inv)
  inv
  }

