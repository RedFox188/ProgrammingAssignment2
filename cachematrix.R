## This function creates a special "matrix" object that can cache its inverse
## the function includes 4 items list: 
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix


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

## cacheSolve n computes the inverse of the special "matrix" returned by makeCacheMatrix. 
##If the inverse has already been calculated, the function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## gets inverse value of the matrix from the previous function
  i <- x$getinverse()
  if(!is.null(i)) { 
    ## checks if the value has been already computed and stored and returns if true
    message("getting cached data")
    return(i)
  }
  ## if there is no cache, gets the data from the previous function
  data <- x$get()
  ## calculates the inverse 
  i <- solve(data, ...)
  ## stores it to the cache 
  x$setinverse(i)
  i
}
