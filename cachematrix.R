# Programming Assignment 2: Lexical Scoping
# Caching the Inverse of a Matrix:
# Matrix inversion is usually a costly computation and there may be some 
# benefit to caching the inverse of a matrix rather than compute it repeatedly.
# The assignment isto write a pair of functions that cache the inverse of 
# a matrix.

#1. makeCacheMatrix: This function creates a special "matrix" object that can
# cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        z <- NULL
        set <- function(y) {
                x <<- y
                z <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) z <<- inverse
        getInverse <- function() z
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}





#2. This function computes the inverse of the special "matrix" created by 
# makeCacheMatrix above. If the inverse has already been calculated (and the 
# matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        z <- x$getInverse()
        if (!is.null(z)) {
                message("getting cached data")
                return(z)
        }
        data <- x$get()
        z <- solve(data, ...)
        x$setInverse(z)
        z
}





