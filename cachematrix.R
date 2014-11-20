## Since matrix inversion is usually costly, these two functions allow for the
## inverse of a matrix to be cached rather than computing it repeatadly. 

## Creates a special matrix that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
  inverted <- NULL
  set <- function(y) {
    x <<- y
    inverted <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(invertedMatrix) inverted <<- invertedMatrix
  getInverseMatrix <- function() inverted
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}

##  Checks if the invers of a matrix 'x' has been previously calculated. 
## If this is true the cached inverted matrix is returned. 
## If not, then the inverted matrix is calculated and returned.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverted <- x$getInverseMatrix()
  if(!is.null(inverted)) {
    message("getting cached inverted matrix")
    return(inverted)
  }
  data <- x$get()
  inverted <- solve(data, ...)
  x$setInverseMatrix(inverted)
  inverted
}
