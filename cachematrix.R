## Function returns the cached inverse of a matrix inform of a list
## containing the matrix and the cached inverse
## otherwise cachesolve solves for theinverse of the matrix
## provided the matrix is invertible.

## makeCache Matrix returns a list containing a matrix object
## and its cached inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix )
}


## Function takes the result of MakeCaheMatrix above, check
## if the inverse of the matrix supplied has been computed
## and returns the cached matrix, otherwise it computes the 
## inverse and returns the result.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}
