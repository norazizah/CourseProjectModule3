## Functions that cache the inverse of a matrix

## Create a special "matrix", which is a list containing
## a function to
##   - set the value of the matrix
##   - get the value of the matrix
##   - set the value of the inverse matrix
##   - get the value of the inverse matrix

## Initialize the inverse property
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ## Method the get the matrix
  get <- function() x
## Way to set the inverse of the matrix
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
## Back a list of the methods
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## Calculate the inverse of the special "matrix" created with the above
## function, reusing cached result if it is available

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
## Compute the inverse of the unique matrix back by "makeCacheMatrix"
## Back to a matrix  “m”
  m <- x$get()
## Compute the inverse via matrix multiplication
  i <- solve(m, ...)
## Set the inverse to the object
  x$setinverse(i)
## Coming back the matrix
  i
}

##command:
##source('cachematrix.R')
##m <- makeCacheMatrix(matrix(c(2, 0, 0, 2), c(2, 2)))
##cacheSolve(m)
## result
##[,1] [,2]
##[1,]  0.5  0.0
##[2,]  0.0  0.5

