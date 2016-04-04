## These two functions calculate and cache the inverse of a a given matrix
## If the inverse is already present in the cache, then it is retrieved.
## If it is not present, then it is calculated and cached
## 

## This function creates a list of functions which set and get 
## the values of the given matrix and its inverse. 

makeCacheMatrix <- function(x = matrix()) {
      
      m <- NULL
      set <- function(y){
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinv <- function(inv) m <<- inv
      getinv <- function() m
      list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## This function checks to see if the inverse is present in the cache,
## and retrives it if it is. If not, it calculates it and caches it.

cacheSolve <- function(x, ...) {
      
      m <- x$getinv()
      if(!is.null(m)){
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinv(m)
      m
      
        ## Return a matrix that is the inverse of 'x'
}
