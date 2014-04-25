## R Programming (John Hopkins)
## Programming Assignment 2
## Submitted by: Anyazelie M. Zephyaire

makeCacheMatrix <- function(myMatrix = matrix()) {
  ## Create a special "matrix" with a list of functions to:
  ## Set & get the value of the matrix and inverse matrix
  iMatrix <- NULL
  set <- function(x) {
    myMatrix <<- x
    iMatrix <<- NULL
  }
  get <- function() myMatrix
  setinverse <- function(i) iMatrix <<- i
  getinverse <- function() iMatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  iMatrix <- x$getinverse()
  if(!is.null(iMatrix)) {
    message("getting cached data")
    return(iMatrix)
  }
  data <- x$get()
  iMatrix <- solve(data, ...)
  x$setinverse(iMatrix)
  iMatrix
}
