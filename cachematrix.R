## The function is for calculate the inverse of a matrix
## but if the inverse has been calculated, the inverse is stored and looked up.

## This function is to create a special 'matrix' to store inverse of input matrix

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(matrix) {
            x <<- matrix
            inv <<- NULL
      }
      get <- function() x
      
      set_inverse <- function(inverse) {
            inv <<- inverse
      }
      get_inverse <- function() inv
      list(set = set, get = get, 
           set_inverse = set_inverse, get_inverse = get_inverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$get_inverse()
      if(!is.null(inv)) {
            message('getting cached inverse')
            return(inv)
      }
      data <- x$get()
      inv <- solve(data)
      x$set_inverse(inv)
      inv
}
