
## creates a list of functions to 1) set value of matrix 2) get value of matrix 
#  3) set value of inverse matrix 4) get value of inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## returns cached value of inverse matrix
## will not function without being called in makecachematrix environment

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv      
}



## the following will test the functions created
b <- matrix(1:4, 2, 2)
c <- makeCacheMatrix(b)
cacheSolve(c)

## Should result in:
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

