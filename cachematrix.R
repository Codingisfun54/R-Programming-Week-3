## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##creates the special matrix object
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                         
  set <- function(y) {                
    x <<- y               
    inv <<- NULL                      
  }
  get <- function() x           
  
  setinverse <- function(inverse) i <<- inverse  
  getinverse <- function() i              
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)   
  
}
## Write a short comment describing this function
## finds the inverse of the special matrix returned by the function above
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
