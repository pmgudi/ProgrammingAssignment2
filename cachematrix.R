## This function creates a special "matrix" object that can cache its inverse

## cacheMatrix - need to describe this better

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
   
     set <- function(y) {
     x <<- y
     m <<- NULL
    }
    get <- function() x
    x<-matrix (rep(1:10), 10,nrow=10, ncol=10)
  
    setinv <- function(solve) m <<- inv
    getinv <- function() m
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


##Inverse Matrix - This function computes the inverse of the special "matrix" returned 
##by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
##solve(x)
## Return a matrix that is the inverse of 'x'
  
    m <- x$getinv()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
  
}

