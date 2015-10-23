## The functions will return inverse of a matrix 
## If the matrix is cached it will retrieve the values 
## from there return the results.
## Example of how to call a function:
## i<- makeCacheMatrix(matrix(c(5,3,2,1), nrow=2, ncol=2))
## cacheSolve(i)


## makeCacheMatrix accepts matrix as argument, then creates
## four functions and returns a list of those functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function(){ 
        x
  }
  setinv <- function(inv) {
     m <<- inv
  }
  getinv <- function() {
    m
  }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## cacheSolve function will take the passed arguments from
## makeCacheMatrix and will return the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
## if cached value exists then return it do not recalculate
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  
  
}
