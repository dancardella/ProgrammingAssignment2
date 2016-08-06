## To establish the Matrx, type these commands:
# x <- matrix(runif(25), 5, 5)
# a<- makeCacheMatrix(x)

makeCacheMatrix <- function(x = matrix()) {
 ## makeVector <- function(x = numeric()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(x) i <<- solve(x)
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
  
## To run this function and complete the exercise, input these commands:
# b<- cacheSolve(a)
# x
# b
## And to verify that the cache is working properly, type this again:
# cacheSolve(a)

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {

  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  }
