## The makeCacheMatrix and cacheSolve functions are used to first create an object
## that stores a matrix, and then cache the inverse of the matrix. First, input your matrix
## into the makeCachMatrix function, and store the output with a variable.  Next, input the 
## the stored value representing the output of the makeCacheMatrix function into the cachSolve
## function, and the output will be the inverse of the matrix.

## The function makeCacheMatrix creates a special matrix with individual functions that:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse matrix
## 4. Get the value of the inverse matrix
## The input of this function is a matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve calculates the inverse of the matrix created with the function
## makeCacheMatrix.  The first step it takes is to see whether the inverse has
## been previously calculated, and if it has been, it skips the step of determining 
## the inverse, and returns the value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data<- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i
}
