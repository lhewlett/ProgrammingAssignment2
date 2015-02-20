## when cacheSolve(makeCacheMatix(x)) for invertable matrix x is called, will return the inverse of matrix x
##if the inverse has already been calculated, will retrieve the inverse from the cache 

## makeCacheMatrix creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL                                      ##clears out any existing inverse solutions
  set <- function(y) {
    x <<- y                                     ##resets x when matrix changes
    m <<- NULL                                  ##resets m to null when matrix changes
  }
  
  get <- function() x                             ##return the value of x in its lexical scope
  setinverse <- function(slv) m <<- slv           ##sets m to value passed when in cacheSolve through x$setInverse(m)
  getinverse <- function() m                      ##returns m
  list(set = set, get = get,                      ##defines the object that the cache can inverse
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSole returns a matrix that is the inverse of matrix x returned by makeCacheMatrix
## if the inverse has already been calculated for the matrix, cacheSolve retreives the inverse from the cache

cacheSolve <- function(x, ...) {
        
  m <- x$getinverse()                 ##subsets list returned in makeCacheMatrix and sets m to the value of getinverse
  if(!is.null(m)) {                   ## if m has been previously calculated returns m from cache
    message("getting cached data")
    return(m)                         ## m will be returned and no other portion of this function will be evaluated, if m has previously been calculated
  }
  data <- x$get()                   ##gets current x from makeCacheMatrix
  m <- solve(data, ...)             ##sets m to the inverse of matrix x, calculated using the solve function       
  x$setinverse(m)                   ##passes the inerse calculated immediately above to makeCacheMatrix, for future reference
  m                                 ##returns the inverse of the matrix
  
}
