## Put comments here that give an overall description of what your
## functions do

## This function takes in a normal matrix and returns
## a list of function references
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    #setting new matrix
    x <<- y
    #invalidate the cached inv
    inv <<- NULL
  }
  
  get <- function(){
    #return the raw matrix
    x
  }
  
  setInverse <- function(inv){
    inv <<-- inv
  }
  
  getInverse <- function(){
    inv
  }
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This will solve the inverse of the "cacheable" matrix
## built from makeCacheMatrix method
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    #cache hit. return value
    message("return cached val")
    return(inv)
  }
  
  #cache miss. get the matrix
  mat <- x$get()
  
  #solve inverse
  inv <- solve(mat)
  x$setInverse(inv)
  inv
}
