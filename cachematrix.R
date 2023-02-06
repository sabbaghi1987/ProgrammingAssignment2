## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function defines a kind of matrix that can store its inverse to avoid
## redundant computation in future if the inverse matrix is needed.

makeCacheMatrix <- function(x = matrix()) {
  inverseMat <- NULL
  set <- function(y) {
    x <<- y
    inverseMat <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inverseMat <<- inverse
  getInv <- function() inverseMat
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function
## This function can cache the inverse of a matrix if pre-computed in the past
## or find its inverse if the inverse matirx is not pre-computed.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
  
  
}
