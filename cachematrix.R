## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  inverseMatrix <- NULL
  
  set <- function (y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  
  get <- function (){
    x
  }
  
  getInverseMatrix <- function () {
    inverseMatrix  
  }
  
  setInverseMatrix <- function (inverse){
    inverseMatrix <<- inverse
  }
  
  list (set=set, get=get, setInverse=setInverseMatrix, getInverse=getInverseMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getInverse()
  
  if (!is.null(inverseMatrix)){
    return (inverseMatrix)
  }
  
  matrix <- x$get()
  inverseMatrix <- solve(matrix)
  x$setInverse(inverseMatrix)
  inverseMatrix
}
