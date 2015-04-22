## Put comments here that give an overall description of what your
## functions do

## stores a matrix and its inverse into cache, gets a matrix and its inverse from cache

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {      ## set takes a matrix argument and stores it in x in the prnt env
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## retrieves the inverse matrix from cache if found, else calculate the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()  # get copy of inverse matrix from parent env cache
      if(!is.null(m)) {   # if it exists, return it without recalc
            message("getting cached data")
            return(m)
      }
      data <- x$get()          # retrieve the original matrix
      m <- solve(data, ...)    # calculate the inverse
      x$setinverse(m)          # store it in cache
      m
}
