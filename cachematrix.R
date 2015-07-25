
## caching the inverse of a matrix
## so that it doesn't have to be calculated again if it is known/calculated already

## create the getter and setter functions for the given matrix, return a list of the functions
## fetches a matrix as argument (here: square and invertible - otherwise "solve" doesn't work in cacheSolve)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                                           # initialize the inverse
  
  # create getters and setters
  set <- function(y){                                   # set matrix to given argument y (not needed here)
    x <<- y                                             ## copy matrix
    inv <<- NULL                                        ## reset the inverse matrix
  }
  get <- function() x                                   # return matrix
  setInv <- function(invX) inv <<- invX                 # set inverse matrix to given argument invX
  getInv <- function() inv                              # return inverse
  
  list(set=set, get=get, setInv=setInv, getInv=getInv)  # return value (list of 4 functions)
}


## get the inverse of the existing matrix - from "cache" or calculate it this (first) time
## fetches a list of functions as argument (result from makeCacheMatrix)
cacheSolve <- function(x, ...) {
  inv <- x$getInv()   ## fetch inverse matrix, returns NULL if not calculated yet
  
  if(!is.null(inv)){  ## already calculated
    message("already calculated, fetched from cache: ")
    return(inv)       ## return existing inverse matrix
  }
  
  # if not yet calculated:
  data <- x$get()     ## fetch (original) matrix for calculation
  message("I need to calculate, wait a minute ... ;)")
  inv <- solve(data)  ## calculate inverse (2nd argument b can be left out)
  x$setInv(inv)       ## save / put matrix in "cache"
  inv                 ## Return a matrix that is the inverse of 'x'
}
