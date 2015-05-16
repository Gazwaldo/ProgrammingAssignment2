## The following is a pair of functions designed to Solve Matrix inverses.
##Furthermore, if an inverse has already been calculated it is stored to the Cache.
##This helps save calculation time in the future by returning the previous calcs.
##To operate first create a matrix x.
##Then set a second variable y to the result of y<-makeCacheMatrix(x).
##Then run cacheSolve(y).
##Run cacheSolve(y) again. This time it will pull the result from the Cache.

## The first function creates a special vector which the second function relies on.

makeCacheMatrix <- function(x = numeric()) {
  
  m <- NULL
  
  set <- function(y) {
    
    x <<- y
    
    m <<- NULL
    
  }
  
  get <- function() x
  
  setinv <- function(inv) m <<- inv
  
  getinv <- function() m
  
  list(set = set, get = get,
       
       setinv = setinv,
       
       getinv = getinv)
  
}

## The second function checks if the inverse has already been calculated.
##If so it will return the stored value. Otherwise it calculates from scratch.

cacheSolve <- function(x, ...) {
  
  m <- x$getinv()
  
  if(!is.null(m)) {
    
    message("getting cached data")
    
    return(m)
    
  }
  
  data <- x$get()
  
  m <- solve(data, ...)
  
  x$setinv(m)
  
  m
  
}
