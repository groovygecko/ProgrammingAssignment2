## This function takes a matrix known as 'x', calculates the inverse matrix and assigns this
## inverse to a global free variable so that it can be utilised at a later time

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(b) {
        a <<- b
        m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function attempts to find a stored copy of the inverse matrix in the global 'm'
## variable. If it is not found then the function calulates the inverse and returns 
## the result.

cacheSolve <- function(x = matrix(), ...) {  
      m <- a$getinverse()
      If(!is.null(m)) {
        message("Returning cached inverse matrix")
        return(m)
      }
      data <- a$get()
      m <- solve(data, ...)
      a$setinverse(m)
      m
}
