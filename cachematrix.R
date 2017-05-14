## Put comments here that give an overall description of what your
## functions do

## This function creates special matrix and caches the inverse of matrix 

makeCacheMatrix <- function(x = matrix()) {
          inv <- NULL
          set <- function(y){
            x <<- y
            inv <<- NULL
          }
          get <- function() x
          setinv <- function(i) inv <<- i
          getinv <- function() inv
          list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Uses the above function to  calculate inverse of matrix

cacheSolve <- function(x, ...) {
        inverse <- x$getinv()
        if(!is.null(inverse))
        {
          return((inverse))
        }
        data <- x$get()
        inverse = solve(data)
        x$setinv(inverse)
        inverse
}
