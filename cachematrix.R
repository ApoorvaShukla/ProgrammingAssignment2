## Following functions are run together to create inverse square matrix

## makeCacheMatrix 

makeCacheMatrix <- function(x = matrix()) {  
  inv <- NULL
  set <- function(y) {      ## set the value of the matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x       ## get the value of the matrix
  setinverse <- function(inverse) inv <<- inverse  ## set the value of inverse of the matrix
  getinverse <- function() inv         ## get the value of inverse of the matrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

x = rbind(c(6, -3.5), c(-7, 6.5))
j = makeCacheMatrix(x)
j$get()

## The following function returns the inverse of the matrix.
## This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  j1 <- x$getinverse()
  if(!is.null(j1)) {                      ## the inverse has already been computed. If so, it gets the result and skips the computation.                          
    message("getting cached data.")
    return(j1)
  }
  data <- x$get()
  j1 <- solve(data)     ## If not, it computes the inverse
  x$setinverse(j1)    ## sets the value in the cache
  j1
}

cacheSolve(j)