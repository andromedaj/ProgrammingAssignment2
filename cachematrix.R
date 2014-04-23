## The two functions makeCacheMatrix and cachesolve are a pair of functions that
## calculate and cache the inverse of a matrix.

## makeCacheMatrix creates a special "matrix" which is a list containing 
## functions to set the value of the matrix, get the value of the matrix,
## set the value of the inverse and get the value of the inverse.

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


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been computed, it gets the inverse from the cache. 
## Otherwise, it computes the inverse of the data and sets the value of the inverse 
## in the cache via the setinverse function.

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
