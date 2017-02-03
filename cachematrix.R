## Both functions demostrate how scoping works while doing inverse of a matrix and caching it

## create a special matrix object which can store both matrix and its inverse 
## it also has functions to set and get both

makeCacheMatrix <- function(x = matrix()) {

  xi <- NULL
  set <- function(y) {
    x <<- y
    xi <<- NULL
  }
  get <- function() x
  setinverse <- function(yi) xi <<- yi
  getinverse <- function() xi
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Function checks if an inverse already cached for a given special matrix 
##    if it exists, then cached inverse is returned
##    if it is not, then it prepares an inverse and caches it for future use

cacheSolve <- function(x,...) {
        ## Return a matrix that is the inverse of 'x'
  matI <- x$getinverse()
  if(!is.null(matI)) {
    message("getting cached data")
    return(matI)
  }
  mat <- x$get()
  matI <- solve(mat, ...)
  x$setinverse(matI)
  matI
  
}
