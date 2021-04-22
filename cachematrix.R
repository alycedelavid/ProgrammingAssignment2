## functions give meaning to the arguments made as it performs
## most of the work to be done. In this assignment I used functions to 
## create a matrix inversion that may be used in caching the inverse of a matrix.

## This function creates a matrix that is a list with a function the would:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse 
## 4. get the value of the inverse

makeCacheMatrix <- function(X = matrix()) {
  inv <- NULL
  set <- function (Y) {
    X <<- Y
    inv <<- NULL
}
  get <- function () {X}
  setInverse <- function (inverse) (inv <<- inverse)
  getInverse <- function ()(inv)
  list (set = set, get=get, setInverse = setInverse, getInverse=getInverse)
}

## This function solves the inverse of the matrix.
## It ensures if the inverse has already been solved by the above function.
## if not, it would solve the inverse and set the value of the
## inverse in the cache using the set inverse function.

cacheSolve <- function(X, ...) {
  inv <- X$getInverse ()
  if(!is.null(inv)){
    message("getting cached data")
    return (inv)
  }
  mat <- X$get()
  inv <- solve(mat, ...)
  X$setInverse (inv)
  inv
}
        ## Return a matrix that is the inverse of 'x'
