## Caching the Inverse of a Matrix

## The objective of the following two functions is to use cache to
## store the result of a previously computed operation (i.e. the 
## inverse of a matrix) thereby, removing the need to run the 
## computation each time it is called, which can be time-consuming.

## The first function, makeCacheMatrix creates a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function (y) {
          x <<- y
          m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set=set, get=get, 
            setinverse=setinverse, 
            getinverse=getinverse)
}

## The following function calculates the inverse of the matrix created with 
## the above function. It first checks to see if the inverse has already
## been calculated. If so, it gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the inverse of the data and sets the 
## resulting value in the cache via the setinverse function.

cacheSolve <- function(x, ...) {   ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if(!is.null(m)) {
      message("getting cached data")
      return(m)
      }
      data <- x$get()
      m <- solve(data,...)
      x$setinverse(m)
      m
}
