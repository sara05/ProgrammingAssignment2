## Functions that cache the inverse of a matrix 
## 
## makeCacheMatrix creates a list containing 
## a function to 
##   1 - set the value of the matrix 
##   2 - get the value of the matrix 
##   3 - set the value of the inverse matrix 
##   4 - get the value of the inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Function below returns the inverse of the matrix 
## created with the function above
## Function will skip computation if inverse has been computed already,
## if not it will compute the invers and set the value in the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
