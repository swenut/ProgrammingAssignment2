## This script contains functions that create the inverse of a given matrix.

## This function creates a special vector

makeCacheMatrix <- function(x = matrix()) {
  ## Assigning the number of rows & colums
  r <- nrow(x)
  c <- ncol(x)
  ## Defining a null matrix with same dimensions
  x_inv <- matrix(,r,c)
  set <- function(y){
    x <<- y
    x_inv <<- matrix(,r,c)
  }
  get <- function() x
  setinverse <- function(solve) x_inv <<- solve
  getinverse <- function() x_inv
  list (set = set, get = get, 
        setinverse = setinverse,
        getinverse = getinverse)
  
}


## This function checks for an inverse in cache before it recalculates it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  x_inv <- x$getinverse()
  if (!all(is.na(x_inv))){
    message("getting cached data")
    return(x_inv)
  }
  data <- x$get()
  x_inv <-solve(data, ...)
  x$setinverse(x_inv)
  x_inv
}



