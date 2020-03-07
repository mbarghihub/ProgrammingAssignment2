## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a function that produce a list of functions and take a matrix as an 
## argument and caches the inverse of this matrix that is produced by cacheSolve function. 
## The main hint in this function is the <<- operator that causes the inverse be cached in 
## enclosing environment   so that it can be gotten in the  future by the cacheSolve function.
##
makeCacheMatrix <- function(x = matrix()){
  inver <- NULL
  set <-function(y){
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inver <<- solve
  getInverse <- function() inver
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve  is a function that uses the same argument (the same matrix that is given to 
## the makeCasheMatrix function as an argument) and checks if the inverse of this matrix is 
## in cache and if it could find the inverse in the cache, it prints a message . 
## If there is not the inverse in the cache , then it calculates the inverse and return it and 
## this inverse is cached in enclosing environment.
##
cacheSolve <- function(x, ...){
    inver <- x$getInverse()
    if(!is.null(inver)){
      message("getting cached data")
      return(inver)
    }
    data <- x$get()
    inver <- solve(data)
    x$setInverse(inver)
    inver
  }