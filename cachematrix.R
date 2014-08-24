makeCacheMatrix <- function(x = matrix()) {
  #INITIALISE INVERSE WITH NULL VALUE
  inve <- NULL
  ## STORE THE MATRIX 
  setMatrix <- function(y){
    x <<- y
    inve <- NULL
  }
  ### RETURN THE STORED VALUE
  getMatrix <- function() x
  ## Store the inverse value
  setInverse <- function(solve){
    inve <<- solve
  }
  ## GET THE STORED INVERSE VALUE
  getInverse <- function() inve
  list(setMatrix=setMatrix,getMatrix=getMatrix,setInverse=setInverse,getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    message("Getting Cached data")
    return(inverse)
  }
  xmat<-x$getMatrix()
  inverse <- solve(xmat)
  x$setInverse(inverse)
  inverse
}
## TESTING THE CODE USING SOME TESTS
a <- makeCacheMatrix( matrix(c(1,2,8,9), nrow =2, ncol =2) );
a$getMatrix()
cacheSolve(a)
a$getInverse()
cacheSolve(a)

