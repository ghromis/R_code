## makeCacheMatrix gets and sets the value of the matrix and 
## gets and sets the inverse of the same matrix.


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv<- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve calculates inverse of the matrix created with the makeCache matrix. 
## It first checks if the inverse has already been calculated, and if it has, it gets 
## the inverse from the cache and skips the computation. Otherwise, it calculates and 
## sets the inverse.

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
      inv
}

# test
# v <- c(2,3, 1, 5, 1, 0, 3, 1, 0, 2, -3, 2, 0, 2, 3, 1)
# x <- matrix(v,4,4)
# m <- makeCacheMatrix(x)
# cacheSolve(m)


