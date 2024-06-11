## Put comments here that give an overall description of what your
## functions do

## Matrix inversion is often a costly calculation so there are sometimes 
## benefits to caching the inverse of a matrix, rather than calculating it
## repeatedly.

## Below are two functions that work together to create a special object that
## can store a matrix, and cache its inverse.

## Write a short comment describing this function

## This function creates a special 'matrix' object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { ## define the argument with default mode of 'matrix'
        inv <- NULL                     ## initialise 'inv' as 'NULL', will hold value of matrix inverse        
        set <- function(y) {            ## define the set function to assign new
                x <<- y                 ## value of matrix in parent environment
                inv <<- NULL            ## if there is a new matrix, reset 'inv' to 'NULL'
        }
        get <- function() x             ## define the 'get' function, returns value of the matrix argument
        setInverse <- function(inverse) inv <<- inverse ## assigns value of 'inv' in parent environment
        getInverse <- function() inv                    ## gets the value of 'inv' when called
        list(set = set,                 ## 'list' allows you to refer to the functions with the '$' operator.
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

## This function below calculates the inverse of the special 'matrix' created
## by 'makeCacheMatrix' above. It first checks whether the inverse has already 
## been calculated, and if so, retrieves it from the cache rather that 
## calculating it again.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) { ## checks to see if the inverse is already cached, if so, prints message.
                message("getting cached data")
                return(inv)
        }
        dat <- x$get()
        inv <- solve(dat, ...)
        x$setInverse(inv)
        inv
}