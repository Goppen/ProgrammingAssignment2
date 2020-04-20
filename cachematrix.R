## These functions allow you to create an object(list) where a matrix is stored and its inverse. They also allow you to 
## compute the inverse. For this to work the matrix has to be square and has to have an inverse.

## This function loads a matrix into a list of functions, you are also able load the inverse of the matrix to it too. 

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


## This function takes as the arguement the list created by 'makeCacheMatrix' and computes the inverse of the matrix stored in it. 
##If the inverse has been previously defined, it just retrieves its value. If the inverse is not present it calculates it to do 
##the computation. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}



 