## the makeCacheMatrix function creates a list of functions in order to 
## set/get the value of a matrix and set/get the value of its inverse
makeCacheMatrix <- function(x = matrix()) {
  if(det(x)==0) print("The determinant is 0, it ain't gonna get inverted")
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The cacheSolve function calculates the inverse of the "matrix" created 
## by the makeCacheMatrix function. However it checks whether it was calculated 
## before. If it was, it will use the cached data instead of calculating it again. 
## If not, it calculates the inverse and sets it using the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  if(det(x)==0) print("The determinant is 0, it ain't gonna get inverted")
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
