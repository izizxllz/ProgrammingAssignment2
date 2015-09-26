# The first function, makeCacheMatrix creates a special "matrix", 
# which is really a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    # initialize inverse matrix value
    inv <- NULL
    
    # set the value of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # get the value of matrix x
    get <- function() x
    
    # set the inverse of matrix
    set_inv <- function(inv_input) inv <<- inv_input
    
    # get the inverse value
    get_inv <- function() inv
    
    list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}


# The following function calculates the inverse of the special "vector"
# created with the above function.
# It first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and sets the value 
# of the mean in the cache via the set_inv function

cacheSolve <- function(x, ...) {
    # first check if the inverse has already been calculated
    inv <- x$get_inv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # else, get the matrix
    data <- x$get()
    
    # calculate the inverse of matrix
    inv <- solve(x, ...)
    
    # cache the inverse
    x$set_inv(inv)
    # Return a matrix that is the inverse of 'x'
    inv
}
