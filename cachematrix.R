## Put comments here that give an overall description of what your
## functions do

## The function creates a special matrix, which containing a list of  functions to
##1.  set the value of the matrix
##2.	get the value of the matrix
##3.	set the value of the Solve
##4.	get the value of the solve


makeCacheMatrix <- function(x = matrix()) {

  s <- NULL
    set <- function(y) {
      x <<- y
      s <<- NULL
    }
    get <- function() x
    setinv <- function(solve) s <<- solve
    getinv <- function() s
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

  
}


## The function get back the inverse  of the special matrix  created with de makeCacheMatrix function.
## it first checks to see if the solve has already been calculated. 
## It uses the Solve function without the "b" argument so it  will return the inverse .

cacheSolve <- function(x, ...) {
  s <- x$getinv()
  if(!is.null(s)) {
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinv(s)
  s
}
