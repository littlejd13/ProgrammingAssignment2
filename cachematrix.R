## establish the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
 invr <- NULL
  set <- function (y) {
    x <<- y
    invr <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) invr <<- solveMatrix
  getInverse <- function() invr
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## solve function that provides final inversse

cacheSolve <- function(x, ...) {
 invr <- x$getInverse()
  if (!is.null(invr)){
    message("getting cached data")
    return(invr)
  }
  data <- x$get()
  invr <- solve(data)
  x$setInverse(invr)
  invr
}
