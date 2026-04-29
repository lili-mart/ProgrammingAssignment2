## Function 1: creates a matrix object that can catch its inverse
## Function 2: computing the inverse of the matrix returned

## makeCacheMatrix: creating a matrix object that can catch its inverse

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y){
  x <<- y
  inv <<- NULL
}
get <- function() x
setinv <- function(inv) inv <<- inv
getinv <- function() inv
list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve: computing the inverse of the matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        inv <- get$setinv()
        if(!is.null(inv)){
          message("getting cache Data")
          return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
        }
