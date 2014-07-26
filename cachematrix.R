## Two functions, for setting a matrix and storing its inverse
## and then creating a function to retrieve the matrix from the 
## cache if know and generate and store it if not previously known

## This function sets the value of the input matrix (setM), generates a function
## to retrieve it (getM), set the inverse (setinvM) and retrieve the inverse (getinvM)

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  setM <- function(y) {
    x <<- y
    j <<- NULL
  }
  getM <- function() x
  setinvM <- function(inv) j <<- inv
  getinvM <- function() j
  list(setM = setM, getM = getM,
       setinvM = setinvM,
       getinvM = getinvM)
}


## This function retrieves a cached matrix inverse if known, by calling makeCacheMatrix,
## and generates and stores, by using setInv1, if it doesn't.

cacheSolve <- function(x, ...) {
  j <- x$getinvM()
  if(!is.null(j)) {
    message("getting cached matrix inverse")
    return(j)
  }
  dataM <- x$getM()
  j <- solve(dataM, ...)
  x$setinvM(j)
  j
}
