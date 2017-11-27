#Creating the makeCachematrix function which creates a list of functions that tdo the following:
  #set the value of the matrix
  #get the value of the matrix
  #set the value of the inverse matrix
  #get the value of the inverse matrix

makeCachematrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  } 
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

#Creating the cacheSolve function which returns the inverse of the matrix created by the makeCachematrix function. 
#If the inverse has already been calculated, this function will return the cached value, otherwise it will calculate
#the inverse matrix and store it.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

#Testing the cacheSolve function.

z <- matrix(c(1,2,3,4), 2, 2)

cacheSolve(makeCachematrix(z))
