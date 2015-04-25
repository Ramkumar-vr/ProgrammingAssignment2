## These two functions are to store the inverse of a matrix in Cache
## and retrieve it from cache if the matrix has not changed

## This function returns a list of functions that can be used to set,
## and get the matrix and also to set and get the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL  ## initiating a variable to store inverse of matrix
  
  ## this function creates the matrix and nullifies the inverse object
  set <- function(y) {
    x <<- y
    invm <<- NULL
  }
  
  ## this function retrieves the matrix
  get <- function() x
  
  ## this function creates the inverse of matrix element
  setinvm <- function(solve) invm <<- solve
  
  ## this function retrieves the inverse of the matrix
  getinvm <- function() invm
  
  ##The list of functions are returned
  list(set = set, get = get,
       setinvm = setinvm,
       getinvm = getinvm)

}


## This function retrieves the inverse of matrix from cache if available
## If the inverse of matrix is not available in cache it determines
## the matrix inverse and updates the element in cache with the value

cacheSolve <- function(x, ...) {
  
  ## retrieves the inverse of matrix if available in cache
  invm <- x$getinvm()
  
  ## if the cache is not null then the cached data is returned
  if(!is.null(invm)) {
    message("getting cached data")
    return(invm)
  }
  
  ## if the cache is null then the matrix inverse is calculated
  ## stored in cache and the inverse of the matrix is returned
  data <- x$get()
  invm <- solve(data, ...)
  x$setinvm(invm)
  
  ## Return a matrix that is the inverse of 'x'
  invm
}
