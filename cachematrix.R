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

## Write a short comment describing this function

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
