#  These funcions will solve a matrix in to it's inverse matrix

# The first function, `makeCacheMatrix` creates a special "matrix", which is
# a list containing a function to

#  1.  set the value of the matrix
#  2.  get the value of the matrix
#  3.  set the value of the inverse of the matrix
#  4.  get the value of the inverse of the matrix 

## This function takes a matrix capable of having an inverse matrix

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

# The cacheSolve function calculates the inverse matrix of the special "matrix"
# created by the makeCacheMatrix function. However, it first checks to see if the
# inverse matrix has already been calculated. If so, it `get`s the matrix from the
# cache it bypasses. Otherwise, it determines the inverse matrix
# in the cache via the `setinverse` function.

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
  i
}
