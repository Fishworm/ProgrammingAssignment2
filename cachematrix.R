## Put comments here that give an overall description of what your
## functions do:

## The following functions are used to create a special object that 
## stores a matrix and caches its inverse. When we need the inverse 
## matrix again, it can be looked up in the cache rather than 
## recomputed. 

## Write a short comment describing this function

## The first function, makeCacheMatrix creates a special "matrix" 
## object that can cache its inverse. It is really a list containing 
## a function to 
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse matrix
## 4.get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinverse <- function(SolveResult) s <<- SolveResult
        getinverse <- function() s
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

## The following function calculates the inverse of the special "matrix" 
## created with the above function. However, it first checks to see if 
## the inverse has already been calculated. If so, it gets the inverse 
## from the cache and skips the computation. Otherwise, it calculates 
## the inverse of the data and sets the value of the inverse in the 
## cache via the setinverse function.

cacheSolve <- function(x, ...) {
        s <- x$getinverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data)          ## Calculate the inverse matrix 
        x$setinverse(s)           ## Cache the inverse matrix
        s                         ## Return the matrix that is the inverse of 'x'
}
