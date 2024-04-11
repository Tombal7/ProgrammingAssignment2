## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Creates a matrix object that caches the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## Computes the inverse of the matrix unless it is already cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
