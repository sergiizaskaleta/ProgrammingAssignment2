## Put comments here that give an overall description of what your
## functions do
##----------------------------------------------------------------
## These two functions makeCacheMatrix and cacheSolve deal with an
## object that can cache the result of a previously completed
## computation, particularly - taking the inverse of a matrix.
## makeCacheMatrix is used to create such an object, and cacheSolve
## is used to calculate (or retrieve saved result) the inverse of
## this matrix.
##----------------------------------------------------------------
## Sample usage:
## > c=rbind(c(1, -1/4), c(-1/4, 1))
## > myMatrix <- makeCacheMatrix(c)
## > myMatrix$get()
## > cacheSolve(myMatrix)
## > cacheSolve(myMatrix) # <~ 2nd call will message "getting cached..."

## Write a short comment describing this function
## ----------------------------------------------
## makeCacheMatrix creates an `object` with four `methods` for working with
## a matrix and its inverse. You can store values with set(y) and
## setinverse(inverse) methods and retrieve stored values with get() and
## getinverse() methods

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    # saves the new value of the matrix and unsets
    # the variable storing the inverse matrix
    x <<- y
    inv <<- NULL
  }
  
  get <- function() {
    # returns saved value of the matrix
    x
  }
  
  setinverse <- function(inverse) {
    # saves the value of the inverse matrix
    inv <<- inverse
  }
  
  getinverse <- function() {
    # returns saved value of the inverse matrix
    inv
  }
  
  # return the list of described methods
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

## Write a short comment describing this function
## ----------------------------------------------
## cacheSolve attempts to retrieve the inverse of the "special" matrix
## (object) if it is already stored and return it. In case it was not
## previously calculated, the function will calculate the inverse,
## store its value for possible future use and then return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # retrieve stored value of the inverse matrix
  inv <- x$getinverse()
  
  # if such value exists
  if(!is.null(inv)) {
    message("getting cached inverse matrix")
    return(inv)
  }
  
  # no inverse saved ~> need to calculate it
  # retrieve the matrix itself and then invert it
  m <- x$get()
  inv <- solve(m) # solve(m) is a function in base R that return the inverse
                  # of a square matrix m
  
  # save the calculated inverse
  x$setinverse(inv)
  
  # return the calculated inverse
  inv
  
}
