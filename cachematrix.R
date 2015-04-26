## 

## This function, makeCacheMatrix creates a special "vector", 
## which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) m <<- inverse
  
  getinverse <- function() m
  
  ## Returns a list of 4 functions.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## This function cacheSolve calculates the inverse of the special "matrix" created 
## with the above function. However, it first checks to see if the inverse has already 
## been calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse 
## in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  
  ## Check if valid inverse already exists. If it does, 
  ## return it. Otherwise, continue and calculate the inverse.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## Retrieve the matrix
  data <- x$get()
  
  ## Calculate inverse of the matrix
  m <- solve(data, ...)
  
  ## Set the inverse of the matrix
  x$setinverse(m)
  
  ## Return the calculated inverse of the matrix.
  m
}
