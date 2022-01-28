## the following functions store the cache of the inverse of a matrix
## so they computed values can be stored and recalled without 
## spending time on more computation

## makeCacheMatrix creates a special matrix which first sets the 
## elements of the matrix, then gets the gets and then sets the 
## elements of the inverse matrix and then gets the elements of the 
## inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  set <- function(y) {
    x <<- y
    z <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) z <<- solve
  getinverse <- function() z
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve calculates the inverse of a matrix using the `solve`
## function but before solving it checks from the makeCacheMatrix
## to see if the inverse is already there - thus, it can obtain 
## the value and skip computation of the inverse

cacheSolve <- function(x, ...) {
  z <- x$getinverse()
  if(!is.null(z)) {
    message("getting cached data")
    return(z)
  }
  data <- x$get()
  z <- solve(data, ...)
  x$setinverse(z)
  z ## Return a matrix that is the inverse of 'x'
}
