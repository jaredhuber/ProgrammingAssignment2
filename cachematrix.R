## When used in sequence, these functions will both determine the inverse of a matrix and cache that 
## value to save on processing time in subsequent operations

## Call makeCache matrix to instantiate a new matrix cache object, which is a list of operations and data
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_saved_value <- function(solved_val){ 
    ## print("caching the value now")
    m <<- solved_val
  }
  get_saved_value <- function() m
  list(set = set, 
       get = get,
       set_saved_value = set_saved_value,
       get_saved_value = get_saved_value
       )
}

## Call cacheSolve on an instance of makeCacheMatris to eitehr retrive a value from cache or solve for the inverse
## of a new matrix 
cacheSolve <- function(x, ...) {
  m <- x$get_saved_value()
  if(!is.null(m)) {
    print("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set_saved_value(m)
  #print(x$get_saved_value())
  m
}


