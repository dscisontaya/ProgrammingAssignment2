## Finding the inverse of a matrix is one of the most common tasks
## while working with linear algebraic expressions.
## In this assignment, I try to create the functions that cache 
## the inverse of a matrix by the following function. 

## makeCacheMatrix: This function creates a special 'matrix' 
## that can cache its inverse.
## It is a list containing a function to;
##      * Set the value of the matrix
##      * Get the value of the matrix
##      * Set the value of the inverse
##      * Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    set_inv <- function(solve) inv <<- solve
    get_inv <- function() inv
    list(set = set, get = get,
         set_inv = set_inv,
         get_inv = get_inv)
}


## This function computes the inverse of the special matrix 
## returned by *makeCacheMatrix*.
## We use the *solve()* function in R to compute the inverse of a matrix.

cacheSolve <- function(x, ...) {
    inv <- x$get_inv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$set_inv(inv)
    inv
}
