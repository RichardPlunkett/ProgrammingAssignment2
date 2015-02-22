# This module defines a special kind of matrix that caches the result of the
# inverse matrix calculation.


makeCacheMatrix <- function(m = matrix()) {

    #  Prepares a special kind of matrix object, that supports caching the results
    #  of inverse matrix calculations.
    #
    #  USAGE: makeCacheMatrix(m) - where 'm' is a matrix
    #
    #  Returns a a special "matrix" object which is really a list which 
    #  contains functions to:
    #  1. set(m) : to set the value of the matrix to be wrapped
    #  2. get() : to get the value of the matrix
    #  3. set.cache() : used by cacheSolver to cache the value of the inverse of the matrix
    #  4. get.cache() : used by cacheSolver to get the cache value

    
    cache <- NULL   # start cache in cleared state

    # now define the functions described above
    set <- function(y) {
        m <<- y
        cache <<- NULL   # clear cache whenever the matrix held changes
    }
    get <- function() m
    set.cache <- function(cache) cache <<- cache
    get.cache <- function() cache
    
    # finally return our list
    list(set = set, get = get,
         set.cache = set.cache,
         get.cache = get.cache)
}


cacheSolve <- function(x, ...) {

    #  Calculates the inverse of a matrix object created with makeCacheMatrix().
    #
    #  USAGE: cacheSolve(x, ...) - where 'x' is a 'makeCacheMatrix' object
    #
    #  Returns a matrix that is the inverse of the one wrapped by 'x'.
    #  It caches the calculated inverse and returns it on subsequent calls if the wrapped
    #  matrix hasn't been changed

    cache <- x$get.cache()   # retrieve from cache  

    if(!is.null(cache)) {
        # cache result exists, return it
        return(cache)
    }

    # cache isn't filled yet
    internal.matrix <- x$get()
    inverse <- solve(internal.matrix, ...) # solve it
    x$set.cache(inverse) # store result
    inverse
}



