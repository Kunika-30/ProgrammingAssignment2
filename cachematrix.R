## This function creates a special "matrix" object that can cache its inverse.
## It is a list containing functions to set/get the matrix and set/get its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # 'inv' will store the cached inverse of the matrix. Initialize it to NULL.
  inv <- NULL
  
  # 'set' function to set the matrix.
  # When a new matrix is set, the cached inverse is cleared by setting inv to NULL.
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # 'get' function to retrieve the matrix.
  get <- function() x
  
  # 'setinverse' function to store the calculated inverse in the cache.
  # This is called by cacheSolve.
  setinverse <- function(inverse) inv <<- inverse
  
  # 'getinverse' function to retrieve the cached inverse.
  getinverse <- function() inv
  
  # Return a list of all the above functions. This is our "special matrix" object.
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" created by makeCacheMatrix.
## It first checks if the inverse has already been calculated. If so, it gets the
## inverse from the cache. Otherwise, it calculates the inverse and caches it.

cacheSolve <- function(x, ...) {
  ## Attempt to get the inverse from the cache using the getinverse function.
  inv <- x$getinverse()
  
  ## If the cached inverse is not NULL, a cached value exists.
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv) # Return the cached inverse and skip computation.
  }
  
  ## If the cache was NULL, we must compute the inverse.
  # Get the matrix from our special object.
  data <- x$get()
  # Calculate the inverse using the solve() function.
  inv <- solve(data, ...)
  # Cache the newly computed inverse for future use.
  x$setinverse(inv)
  
  ## Return the newly computed inverse.
  inv
}