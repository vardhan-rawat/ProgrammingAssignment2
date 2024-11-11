## These functions work together to cache the inverse of a matrix.
## The caching mechanism can optimize performance by avoiding repeated 
## calculations of the inverse, which is particularly useful for large matrices.

## This function creates a special "matrix" object that can cache its inverse.
## It contains functions to:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize the inverse as NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL  # Reset the inverse when the matrix is set
    }
    get <- function() x  # Return the matrix
    setInverse <- function(inverse) inv <<- inverse  # Cache the inverse
    getInverse <- function() inv  # Return the cached inverse
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix. If the inverse has already been calculated (and the matrix 
## has not changed), then it retrieves the inverse from the cache.


cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {  # Check if the inverse is already cached
        message("getting cached data")
        return(inv)  # Return cached inverse
    }
    data <- x$get()  # Get the matrix from the special 'matrix' object
    inv <- solve(data, ...)  # Calculate the inverse matrix
    x$setInverse(inv)  # Cache the inverse matrix
    inv  # Return the inverse matrix
}
