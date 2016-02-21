# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
      inver <- x$getinverse()
      if(!is.null(inver)) {
            message("getting cached data")
            return(inver)
      }
      data <- x$get()
      inver <- solve(data, ...)
      x$setinverse(inver)
      inver
}

## Sample run:
## a <- diag(6,3)
##      [,1] [,2] [,3]
## [1,]    6    0    0
## [2,]    0    6    0
## [3,]    0    0    6
## CachedMarix <- makeCacheMatrix(a)
## cacheSolve(CachedMarix)
 ##          [,1]      [,2]      [,3]
## [1,] 0.1666667 0.0000000 0.0000000
## [2,] 0.0000000 0.1666667 0.0000000
## [3,] 0.0000000 0.0000000 0.1666667
##      [,1] [,2] [,3]
## [1,]  0.2  0.0  0.0
## [2,]  0.0  0.2  0.0
## [3,]  0.0  0.0  0.2
##b <- diag(2,6)
##b
##      [,1] [,2] [,3] [,4] [,5] [,6]
## [1,]    2    0    0    0    0    0
## [2,]    0    2    0    0    0    0
## [3,]    0    0    2    0    0    0
## [4,]    0    0    0    2    0    0
## [5,]    0    0    0    0    2    0
## [6,]    0    0    0    0    0    2
##CachedMarix <- makeCacheMatrix(b)
##cacheSolve(CachedMarix)     
##      [,1] [,2] [,3] [,4] [,5] [,6]
## [1,]  0.5  0.0  0.0  0.0  0.0  0.0
## [2,]  0.0  0.5  0.0  0.0  0.0  0.0
## [3,]  0.0  0.0  0.5  0.0  0.0  0.0
## [4,]  0.0  0.0  0.0  0.5  0.0  0.0
## [5,]  0.0  0.0  0.0  0.0  0.5  0.0
## [6,]  0.0  0.0  0.0  0.0  0.0  0.5
##cacheSolve(CachedMarix)   #getting cached data
## getting cached data
##      [,1] [,2] [,3] [,4] [,5] [,6]
## [1,]  0.5  0.0  0.0  0.0  0.0  0.0
## [2,]  0.0  0.5  0.0  0.0  0.0  0.0
## [3,]  0.0  0.0  0.5  0.0  0.0  0.0
## [4,]  0.0  0.0  0.0  0.5  0.0  0.0
## [5,]  0.0  0.0  0.0  0.0  0.5  0.0
## [6,]  0.0  0.0  0.0  0.0  0.0  0.5
