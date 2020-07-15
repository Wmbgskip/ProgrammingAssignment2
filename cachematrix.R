## These functions make a matrix, cache it, and then
## calculate the inverse matrix of the cached matrix
## (if it exists)

## make the matrix, invert it, cache it

makeCacheMatrix <- function(x = matrix()) {
      m<- NULL

      ## force matrix to start out empty
      set <- function(y) {
            x <<- y
            m <<- NULL
      }

      ## create matrix with new values
      get <- function() x

      ## invert the matrix      
      setInverse <- function(solve) m <<- solve

      ## store the inverted matrix
      getInverse <- function() m
      list(set = set, get = get,
           setInverse = setInverse, getInverse = getInverse)
}

## If the inverted matrix is cached, recall it,
## otherwise invert the matrix, then cache it

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'

      ## check and see if the matrix already exists and is inverted,
      ## if so, return the cache
      m <- x$getInverse()
      if(!is.null(m)){
            message("getting cached data")
            return(m)
      }
      
      ## if the cache is empty, then calculate the inverse and return it 
      data <- x$get()
      m <- solve(data, ...)
      x$setInverse(m)
      m
}