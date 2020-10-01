## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      ## First I set the imput x as a matrix
      ## I give "a", the solved value as NULL
 
      a <- NULL
      set <- function(y){
            x <<- y
            a <<- NULL
      }
      get <- function()x
      setInverse <- function(inverse) a <<- inverse
      getInverse <- function() a
      list(set = set, 
           get = get, 
           setInverse = setInverse, 
           getInverse = getInverse)
}

##-----------------------------------------------------------
## Write a short comment describing this function

##This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      a <- x$getInverse()
      if(!is.null(a)){
            message("getting cached data")
            return(a)
      }
      mat <- x$get()
      a <- solve(mat,...)
      x$setInverse(a)
      a
      }
