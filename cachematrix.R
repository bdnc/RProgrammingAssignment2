# R programming Coursera. Programming Assignment 2.

#These functions check whether the inverse of a matrix has already been
#calculated and is in cache, in which case the value is retrieved from there to
#save recalculating the value. If the value is not available in cache, it is
#calculated and returned.
#Note that the matrix must be square.

# The function makeCacheMatrix returns a list of four functions
# 1 set the matrix
# 2 get the matrix
# 3 set the inverse of the matrix
# 4 get the inverse of the matrix
# some subfunctions use <<- to assign values to variables in parent environment
makeCacheMatrix <- function(matrix = matrix()) {
        # create a variable inv for the inverse and set to NULL initially
        inv <- NULL
        # create set function which allows the matrix to be assigned a new value
        # this function resets inv to NULL so that the inverse is recalculated
        set <- function(y) {
                matrix <<- y
                inv <<- NULL
        }
        # create get function which simply returns the matrix
        get <- function() matrix
        # create set function which assigns a new value to inv
        setinv <- function(i) inv <<- i
        # create getinv function which simply returns the inverse matrix inv
        getinv <- function() inv
        # create a list with each of the four functions
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# The function cacheSolve takes the output of makeCacheMatrix
# and returns the inverse of the original matrix
# If the inverse is available in cache, it will return that.
# If not, it will calculate the inverse and return it.
cacheSolve <- function(x, ...) {
        # retrieve the current value of inv
        current_inv <- x$getinv()
        # if current_inv is not null, return its (cached) value
        if(!is.null(current_inv)) {
                message("retrieving cached data")
                return (current_inv)
        }
        # return(x) above ends the function cacheSolve so nothing else happens
        # however, if inv is null, retrieve the matrix
        matrix <- x$get()
        # then calculate the inverse and assign it to current_inv
        current_inv <- solve(matrix, ...)
        # set assign current_inv to inv so that it's available in cache
        x$setinv(current_inv)
        # return current_inv
        current_inv
}