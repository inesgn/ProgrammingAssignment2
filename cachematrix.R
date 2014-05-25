## Two related functions that create a 'matrix' object that is able to cache its own inverse.
## This avoids to compute the inverse (which can be a time-consuming operation) if it already exists.

## The first function creates the 'matrix' structure that associates the matrix and its inverse. 
# Arguments: x, a matrix.
# Returns: a 4-element list.
# Note: <<- operator is used to preserve state.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        # Returns a list
        return (list(set = set, get = get,
             setinv = setinv,
             getinv = getinv))
}


## The second function returns the inverse of a 'matrix' -as returned by makeCacheMatrix-.
## It first checks if the inverse already exists in 'x'.
# Arguments: x, a 'matrix' type object returned by makeCacheMatrix.
# Returns: inverse of x; a 'getting cached data' message if it already exists.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        # Returns the inverse
        return(inv) 
}
