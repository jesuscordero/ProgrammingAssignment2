## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## "makeCacheMatrix" is a list of functions to create, store and show a 
## special matrix, and, store and show the inverse of the special 
## matrix.
makeCacheMatrix <- function(x = matrix()) {

        ## "cache" initialy set as NULL, for new setting matrix
        cache <- NULL
  
        ## "setmatrix" takes the argument of "makeCacheMatrix" 
        ## and stores it
        setmatrix <- function(y){
                    x <<- y
                cache <<- NULL
        }
        ## "getmatrix" shows the "makeCacheMatrix" argument
        getmatrix <- function(){
                    x
        }
        ## "setinverse" store the inverse of the set it matrix with
        ## "makeCacheMatrix"
        setinverse <- function(z){
                cache <<- z
        }
        ## "getinverse" shows the inverse of "makeCacheMatrix" argument
        getinverse <- function(){
                cache
        }

        list(setmatrix = setmatrix, getmatrix = getmatrix, 
             setinverse = setinverse, getinverse = getinverse)
        ## Creating a list, we can access separately to the result of
        ## each function set it before 
}

## Write a short comment describing this function
## "cacheSolve" use the functions stated in the "makeCacheMatrix"
## function, to show the special matrix inverse cached in "getinverse" 
## and if it´s a new matrix, calculate and show the new inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cache <- x$getinverse()
        if(!is.null(cache)){
          message("getting cached inverse")
          return(cache)
        }
        a <- x$getmatrix()
        cache <- solve(a,...)
        x$setinverse(cache)
        cache
}