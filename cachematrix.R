## Very similar to the makeVector example function, makeCacheMatrix
##  creates a list of functions to act on the matrix argument. We
##  can set the value of the matrix, get the value of the matrix,
##  set the matrix inverse, and grab the matrix inverse.
## In our second function, we can take our special "matrix" from 
##  our first function and either grab the already cached inverse
##  matrix or calculate and store the inverse matrix.

## Here we will create a special "matrix" with our input data,
##  generating a list able to set the matrix, get the matrix,
##  set the matrix inverse, and get the matrix inverse. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## set matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## get matrix
  get <- function() x
  ## set matrix inverse
  setinv <- function(solve) m <<- solve
  ## get matrix inverse
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function will take our special "matrix," the list of functions,
##  and attempt to retrieve a cached inverse matrix. If it does not
##  exist, cacheSolve will run the functions on the original matrix
##  to generate the inverse matrix, cache it, and return it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## get inverse matrix from special "matrix"
  m <- x$getinv()
  if(!is.null(m)) {
        message("getting cached data")
        return(m)
  }
  ## get data to calculate inverse matrix
  data <- x$get()
  ## calculate inverse matrix
  m <- solve(data)
  ## set inverse matrix
  x$setinv(m)
  ## return inverse matrix
  m
}
