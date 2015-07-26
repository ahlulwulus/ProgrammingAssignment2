# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
  # inv will store the cached inverse matrix
  inv <- NULL
  
  # Setter for the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # Getter for the matrix
  get <- function() x
  
  # Setter for the inverse
  setinv <- function(inverse) inv <<- inverse
  # Getter for the inverse
  getinv <- function() inv
  
  # Return the matrix with our newly defined functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve 
# the inverse from the cache.

cacheSolve <- function(x, ...) 
{
  inv <- x$getinv()
  
  # If the inverse is already calculated, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # The inverse is not yet calculated, so we calculate it
  data <- x$get()
  inv <- solve(data, ...)
  
  # Cache the inverse
  x$setinv(inv)
  
  # Return it
  inv
}