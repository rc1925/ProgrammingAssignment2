## below 2 functions can be used to create a concise object, which is capable of
## containing a matrix and its inverse

## the makeCacheMatrix() function returns a list "object" with methods capable of
## storing a returning a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve is a function which sets the inverse of the matrix
## encapsulated in a list returned by the makeCacheMatrix function


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'  
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  
  x$setinverse(i) 
  return(i)
}


##below listed a few commands to illustrate the usage of
##above functions

#mx <- makeCacheMatrix()
#mx$set(matrix(1:4, 2, 2))
#mx$get()
#cacheSolve(mx)
#mx$getinverse()