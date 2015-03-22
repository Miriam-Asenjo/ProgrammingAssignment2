## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  #initialize the value of inverseMatrix variable  
  inverseMatrix <- NULL
  
  ## set function assign the matrix parameter to the x variable that keeps the matrix  
  set <- function (y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  
  ## get function returns the matrix
  get <- function (){
    x
  }
  
  ## getInverseMatrix function returns the cache inverseMatrix
  getInverseMatrix <- function () {
    inverseMatrix  
  }
  
  ## setInverseMatrix function assign the inverse matrix parameter to the variable inverseMatrix that keeps the inverseMatrix
  setInverseMatrix <- function (inverse){
    inverseMatrix <<- inverse
  }
  
  ## returns a list with the operations that the special "matrix" supports: set, get, setInverse, getInverse
  list (set=set, get=get, setInverse=setInverseMatrix, getInverse=getInverseMatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getInverse()
  
  ## If the inverseMatrix is not null then it had been already calculated, the inverseMatrix is returned
  if (!is.null(inverseMatrix)){
    return (inverseMatrix)
  }
  
  ## If the inverseMatrix is null then it hadn´t been already calculated
  ## The value of the matrix is obtained
  matrix <- x$get()
  ## The inverseMatrix of matrix is calculated using the function solve
  inverseMatrix <- solve(matrix)
  ## The inverseMatrix is set using setInverse function
  x$setInverse(inverseMatrix)
  ## The inverseMatrix is returned
  inverseMatrix
}
