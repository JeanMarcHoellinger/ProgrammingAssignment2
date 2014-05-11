## Put comments here that give an overall description of what your
## functions do

## The purpose of these functions is to save time calculating the inverse of 
## matrices by caching the inverted matrix (if computation previously done) and 
## returning the cached matrix to skip the computation

## Write a short comment describing this function

## This function creates a special "matrix", which is a list containing a 
## function to :
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverted matrix
## 4. get the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setSolve <- function(Solve) im <<- Solve
        getSolve <- function() im
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## Write a short comment describing this function

## This function calculates the inverse of the special "matrix" created with 
## the above function. It first checks to see if the inverse of the matrix has 
## already been calculated. If so, it  gets the inverse of the matrix from the 
## cache and skips the computation. Otherwise, it calculates the inverse of the 
## matrix and sets the value of the inverted matrix in the cache via the setSolve
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getSolve()
        if(!is.null(im)) {
                message("getting cached matrix")
                return(im)
        }
        sm <- x$get()
        im <- solve(sm)
        x$setSolve(im)
        im
}
