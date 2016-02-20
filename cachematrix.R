## These functions return(computes) the inverse of a matrix and stores it in cache, if the input is a matrix
## and the matrix is invertable. Furthermore it will retrieve the inverse
## from cache if the inverse of the given matrix is already in cache


## this is our "special" matrix, it has 4 functions, get, set, getinverse and setinverse
## get, returns the matrix, set: set the the whole matrix
## setinverse, sets the inverse of the given matrix
## getinverse, retrieves the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    if(is.matrix(y))
    {
      x <<- y
      m <<- NULL
    }
    else
    {
      m <<- NULL
      message("You did not provide a matrix")
    }
  }
  get <- function() x
  setinverse <- function(matrix) m <<- matrix
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" 
## created with the above function. However, it first checks to see if the inverse has already been calculated, 
## it does so by seeing if there is a not null value when getting the inverse: x$getinverse()
## If so, it gets the inverse from the cache and skips calline solve
## here we check first if the matrix is invertable, not really needed but if it is, then the solve function retrieves the inverse


cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse matrix")
    return(m)
  }
  data <- x$get()
  if(det(data) != 0)
  {
    m <- solve(data, ...)
  }
  else{
    message("this matrix is not invertable")
    return(NULL)
  }
  x$setinverse(m)
  m
}
