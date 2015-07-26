## Michelle Gitgit Assignment #2

## This creates the cacheMatrix function

makeCacheMatrix <- function(x = matrix()) {  ##names function as makeCacheMatrix
       cm <- NULL ##sets cm as Null
       setmat <- function(y) {
              x <<- y
              cm <<- NULL
       }
       getmat <- function() x
       setinverse <- function(inverse) cm <<- inverse
       getinverse <- function() cm
       list(setmat = setmat, getmat = getmat,
            setinverse = setinverse,
            getinverse = getinverse)
}


## This creates the cacheSolve matrix that stored the inverse

cachesolve <- function(x, ...) {
       cm <- x$getinverse()
       if(!is.null(cm)) {
              message("getting cached data")
              return(cm)
       }
       data <- x$getmat()
       cm <- inverse(data, ...)
       x$setinverse(cm)
       cm
}