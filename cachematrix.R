## This function would create a special "matrix" object that can cache its inverse
## (it is actually creating a list with four functions)


makeCacheMatrix <- function(x = matrix()) {
  
  ## creates m and sets it to NULL
  m <- NULL
  
  ## create 'set' function that will save the values of m and x
  ## taking argument 'y' and setting that as the x value, and 
  ## initialising m to NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## create a function 'get' that will return the value of x when called
  ## the value of argument "x" is now stored in $get
  get <- function() x
  
  ## creates a setinverse function that will set the value of m to the inverse
  setinverse <- function(inverse) m <<- inverse
  
  ## creates a getinverse function that will return the value of m
  getinverse <- function() m
  
  ## returns a list with each of the new functions allocated to their name
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This part of the function calculates the inverse of an invertable matrix 
## returned by makeCacheMatrix above.  If the inverse has already been computed
## for this matrix, then cacheSolve will return the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## First, save the $getinverse value for this list to m so later it can check it.
  m <- x$getinverse()
  
  ## if m in not NULL, the code has already calculated the inverse
  if(!is.null(m)) {
    
    ## this informs the user that the system has the inverse stored in the cache
    message("getting cached data")
    
    ## this prints m, which contains the inversed matrix retrieved from the cache
    return(m)
  }
  
  ## this part of the code will be executed if m = NULL (meaning the system has
  ## not computed the inverse of this matrix)
  data <- x$get()
  
  ## the solve function is used to compute the inverse, and the result is stored in m
  m <- solve(data, ...)
  
  ## store the newly calculated inverse to 'm' using the 'setinverse' function
  x$setinverse(m)
  
  ## prints the value of m
  m
}
