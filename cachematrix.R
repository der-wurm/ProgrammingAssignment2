
## caching the inverse of a matrix so that it doesn't have to be calculated again if it is known already


## create a matrix with its getter and setter functions, return a list of them
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                                # initialize the inverse
  set <- function(y){
    x <<- y       # copy matrix
    inv <<- NULL
  }
  get <- function() x                        # return matrix
  setInv <- function(invX) inv <<- invX      # set inverse to given argument
  getInv <- function() inv                   # return inverse
  
  list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## get the invertible of the existing matrix, get it from "cache" or calculate it this (first) time
cacheSolve <- function(x, ...) {
  inv <- x$getInv()  ## look for inverse, fetch value, returns NULL if not calculated yet
  if(!is.null(inv)){
    message("already calculated, fetched from cache: ")
    return(inv)
  }
  data <- x$get()     ## fetch matrix for calculation
  message("I need to calculate, wait a minute ... ;)")
  inv <- solve(data)  ## calculate inverse (b can be left out then)
  x$setInv(inv)       ## save / put matrix in "cache"
  inv                 ## Return a matrix that is the inverse of 'x'
}
