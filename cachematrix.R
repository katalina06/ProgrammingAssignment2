## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { ## function get a matrix
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) { ## function get a matrix
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i) ## Return a matrix that is the inverse of 'x'
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

## Test

A <- matrix(c(2,4,6,8),2,2)
B <- makeCacheMatrix(A)
cacheSolve(B) 
