## cachematrix.R contains functions that will return the inverse of a matrix
## from cache if the inverse is already calculated.  If not already calculated,
## it calculates the inverse of the matrix and returns the inverse.

## makeCacheMatrix first initializes "x" as an empty matrix and "a" to NULL to
## clear things out before before moving to the "set" function. The "set" 
## function establishes the argument "y" and the assigns "y" to "x". 
## It then resets "a" to NULL to clear out a previous cache from cacheSolve if 
## necessary.  

makeCacheMatrix <- function(x = matrix()) {
      a <- NULL
      set <- function(y) {
            x <<- y
            a <<- NULL
      }
      ## This section assigns an empty function as "getting" "X". Next, it 
      ## defines the "setter" of "a" as "setInverse" followed by the "getter" 
      ## for "a" as "getInverse". Finally, it creates a new list() object with 
      ## each element being one of the functions.
      
      get <- function() x
      setInverse <- function(inverse) a <<- inverse
      getInverse <- function() a
      list(set = set, get = get, 
           setInverse = setInverse, getInverse = getInverse)
}

## This function first establishes a function with a single argument so it can
## receive additional arguments called to it. It then attempts to pull the
## inverse for the object passed in "x".  If the result is NULL, it calculates
## the inverse. If it isn't NULL, it pulls the result from the cache.
## Either way, it prints the inverse.  If no inverse is possible, it prints NULL.

cacheSolve <- function(x, ...) {
      a <- x$getInverse()
      if(!is.null(a)) {
            message("getting cached data")
            return(a)
      }
      mtx <- x$get()
      a <- solve(mtx, ...)
      x$setInverse(a)
      a
}
