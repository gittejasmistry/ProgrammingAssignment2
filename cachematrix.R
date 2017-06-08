# makeCacheMatrix() - creates object which enables caching of inverse of matrix
# x is the matrix which we will pass as arguement
# i is inverse matrix which we will calculate or set
# setinverse() is function to set inverse of matrix
# getinverse is function to get inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#cacheSolve() - calculates and sets inverse of a matrix in object of makeCacheMatrix
# x is makeCacheMatrx Object
# i is inverse matrix of matrix stored in makeCacheMatrix object

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
