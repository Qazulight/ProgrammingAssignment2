x <- rbind(c(1, -1/4), c(-1/4, 1))##copy this in and just run z <- makeCacheMatrix(x)
## Makes a function that makes a function that can be called and then the matrix inverted

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {##x is initialized as a function argument
  mTx <- NULL
  setM <- function(y) {
    x <<- y ##assign the input argument to the x object in the parent
    mTX <<- NULL ##assign the value NULL to the m object in the parent
    ## if there is already a valid inverted matric  cached in m, whenever x is reset
    ## the value of m cached in the memory of the object is cleared
    ##, forcing subsequent calls to cachement() to recalculate the inverted matrix
    ## rather than retrieving the wrong value from cache.
    ## the set() funtion does exactly what the first two lines of makeCacheMatrix
    ##does.   
    }
  getM <- function() x ##this is the getter for x if you put makeCacheMatrix in z, z$get retrieves the matrix
  setInverted <- function(solve) mTx <<- solve ##defines setter for matrix m
  getInverted <- function() mTx ##getter for mTx
  list(setM = setM, getM = getM,##makes each of these functions as an element in a list 
       ## and returns it to the parent enviroment
       setInverted = setInverted,
       getInverted = getInverted)
  
  
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
  mTx <- x$getInverted()
  
  if(!is.null(mTx)) {
    message("getting cached data")
    return(mTx)
  
  }
  data <- x$getM()
  mTx <- solve(data, ...)
  x$setInverted(mTx)
  mTx
}

