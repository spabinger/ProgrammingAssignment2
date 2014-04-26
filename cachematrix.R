## Two main functions in this script:
## makeCacheMatrix
##   creates a cached matrix "object"; allows to store matrix and inversed matrix
##
## cacheSolve
##   returns an inversed matrix object either from cache or newly computed


##
## creates a cached matrix object
## getter and setter for matrix
## functions to set the inverse of a matrix
##
makeCacheMatrix <- function(matrix = matrix()) {
  inverseMatrix <- NULL
  
  # setter
  set <- function(y) {
    matrix <<- y
    inverseMatrix <<- NULL
  }
  
  # getter
  get <- function() matrix
  
  # set the inverse of the matrix
  setInverse <- function(inverse) inverseMatrix <<- inverse
  
  # get the inverse of the matrix
  getInverse <- function() inverseMatrix
  
  # put all in a list
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##
## First tries to get the inverse matrix from cache
## if not possible calculates it and then stores it in the cache
##
cacheSolve <- function(x, ...) {
  matrix <- x$getInverse()
  if(!is.null(matrix)) {
    message("getting cached inverse matrix")
    return(matrix)
  }
  data <- x$get()
  matrix <- solve(data, ...)
  x$setInverse(matrix)
  matrix  ## Return a matrix that is the inverse of 'x'
}




## Testing code
a <- makeCacheMatrix(matrix(1:4,2))
a$get()
a$getInverse()
a$set(matrix(5:8,2))
cacheSolve(a)
cacheSolve(a)
a$getInverse()




## Example from course

makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}