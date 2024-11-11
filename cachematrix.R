# makeCacheMatrix function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
  s <- NULL  # Initialize the variable 's' as NULL to store the cached inverse matrix
  
  # set function assigns a new matrix to 'x' and resets the cached inverse 's' to NULL
  set <- function(y) {
    x <<- y    # Assign the new matrix 'y' to the global variable 'x'
    s <<- NULL  # Reset the cached inverse matrix 's' to NULL as the matrix has changed
  }
  
  # get function returns the current matrix 'x'
  get <- function() x  
  
  # setsolve function caches the inverse matrix 'solve' by assigning it to 's'
  setsolve <- function(solve) s <<- solve
  
  # getsolve function returns the cached inverse matrix 's'
  getsolve <- function() s  
  
  # Return a list of the four functions: set, get, setsolve, and getsolve to be used externally
  list(set = set, get = get, 
       setsolve = setsolve, 
       getsolve = getsolve)
}

## cacheSolve function computes the inverse of the matrix if not cached; if cached, retrieves it
cacheSolve <- function(x, ...) {
  s <- x$getsolve()  # Retrieve the cached inverse matrix 's' from the object 'x'
  
  # If the inverse is already cached (s is not NULL), return the cached inverse
  if(!is.null(s)) {
    message("getting inversed matrix")  # Print a message indicating the cache is being used
    return(s)  # Return the cached inverse matrix
  }
  
  data <- x$get()  # Retrieve the matrix 'x' from the object
  s <- solve(data, ...)  # Compute the inverse of the matrix using solve() function
  x$setsolve(s)  # Cache the computed inverse by setting it in the object 'x'
  s  # Return the computed inverse matrix
}
