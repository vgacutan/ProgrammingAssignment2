## Put comments here that give an overall description of what your
##NOTE:  The code below where patterned based on the makeVector and cachmean function provided
## in the programming assignment 2: Lexical Scoping.
## functions do:
## this function will set and cache the input matrix and always assume that the input is a square matrix
## and its inverse can be calculated.
## Write a short comment describing this function
## the fucntion initialize the input matrix container to NULL
## this will set the value of the matrix
## this will set the value of the matrix
## this will set the value of the inverse of the matrix
## this will get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  matrxInverse = NULL
  set = function(y) {
    x<<-y
    matrxInverse<<-NULL
  }
  get = function() x
  setMatrxInverse = function(inverse) matrxInverse <<- inverse
  getMatrxInverse = function() matrxInverse
  list(set=set, get = get,
       setMatrxInverse = setMatrxInverse,
       getMatrxInverse = getMatrxInverse)
}


## Write a short comment describing this function
## this will calculated the inverse of the matrix output by the makeCacheMatrix.
## it will check also if the inverse has been calculated by the if statment below.  It it
## has been calculated, it will just return the value.  if not if will get the date from the
## makeCacheMatrix and calculate the inverse using the function solve().
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matrxInverse = x$getMatrxInverse()
  if (!is.null(matrxInverse)) {
    message("getting cached data")
    return(matrxInverse)
  }
  data <- x$get()
  matrxInverse <-solve(data, ...)
  x$setMatrxInverse(matrxInverse)
  matrxInverse
}

