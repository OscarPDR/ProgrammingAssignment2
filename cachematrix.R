# The following functions are designed to return the inverse of a matrix,
# always assuming the matrix is invertible. As solve() can be costly in
# computation terms, it first checks if the inverse of the given matrix
# has already been calculated. If so, it returns the cached result, otherwise
# it calculates it and stores the result in the cache for a later request.

# makeCacheMatrix(matrix) creates a list containing a function to:
#   1. set the value of the matrix
#   2. get the value of the matrix
#   3. set the value of inverse of the matrix
#   4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() x
    set_inverse <- function(inverse) inverse <<- inverse
    get_inverse <- function() inverse
    list(
        set = set, 
        get = get,
        set_inverse = set_inverse,
        get_inverse = get_inverse
    )
}


# When called, it checks if the inverse of the matrix is already calculated.
# If so, it returns the cached calculation and skips the rest, otherwise, it
# calls the solve(matrix) function and returns the result
cacheSolve <- function(x, ...) {  
    inverse <- x$get_inverse()
    
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    data <- x$get()
    inverse <- solve(data)
    x$set_inverse(inverse)
    
    ## Return a matrix that is the inverse of 'x'
    inverse
}
