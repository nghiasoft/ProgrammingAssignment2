## Assignment: Programming Assignment 2: Lexical Scoping
## Author: Nghia Dai Tran

## Input: Matrix 'x', default of 'x' is an empty matrix.
## Output: A cacheable matrix.
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    error_message <- NULL
    
    set <- function(y) {
        x <<- y
        inverse <<- NULL
        error_message <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setsolve <- function(sol) {
        inverse <<- sol
    }
    
    getsolve <- function() {
        inverse
    }
    
    getError <- function() {
        error_message
    }
    
    setError <- function(error) {
        error_message <<- error
    }
    
    list(set = set,
         get = get,
         setsolve = setsolve,
         getsolve = getsolve,
         setError = setError,
         getError = getError)
}

## Output:
    ## A matrix that is the inverse of 'x'.
    ## NULL if happens error.
## Input:
    ## A cacheable Matrix
## Calculate the inverse of matrix 'x' and cache
## it at the first calculation time. Return 
## the cache value at second calculation time.
## also cache the error if happens during calculate
## inverse.
cacheSolve <- function(x, ...) {
    inverse <- x$getsolve()
    
    if (!is.null(inverse)) {
        message("getting cached solve")
        return(inverse)
    }
    
    error_message <- x$getError()
    if (!is.null(error_message)) {
        message("having a cached error")
        return(NULL)
    }
    
    square_matrix <- x$get()
    tryCatch({
        inverse <- solve(square_matrix, ...)
    }, warning = function(war) {
        error_message <- war
        x$setError(error_message)
    }, error = function(err) {
        error_message <- err
        x$setError(error_message)
    }, finally = {
    })
    
    x$setsolve(inverse)
    inverse
}