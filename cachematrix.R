## This file provides several functions which create a special 'matrix' object
## that holds itself and an optional cache of its inverse.

## This function creates a special "matrix" object that holds itself and an
## optional cache of its inverse.

# we use a real R matrix internally
makeCacheMatrix <- function(x = matrix()) {

  # initialize the inv variable to be NULL - this is retained due to lexical
  # scoping
  inv <<- NULL
  
  #get and set the R matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  #get and set the inverse of the R matrix
  getinverse <- function() inv
  setinverse <- function(inverse) inv <<- inverse
  
  #construct and return our special 'matrix' object
  list(
    set = set, get = get, getinverse = getinverse, setinverse = setinverse
  )
}


## This function computes the inverse of the special 'matrix' returned by
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve will retrieve the
## inverse from the cache.

# please note that no error handling is performed

cacheSolve <- function(x, ...) {

  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  
  # check to see if the inverse has already been calculated
  if(!is.null(m)) {
    message('getting cached data')
    # if it has perform the cached return
    return(m)
  }
  
  # else solve for the inverse
  m <- solve(x$get())
  
  # then cache the solved matrix
  x$setinverse(m)
  
  # finally return our result
  m
  
}
