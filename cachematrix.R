## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    j <- x$getinverse()
    if (!is.null(j)) {
      message("getting cached data")
      return(j)
    }
    data <- x$get()
    j <- solve(data, ...)
    x$setinverse(j)
    j
  }


# Testing the function 
A <- matrix(4:7,2,2)

## Using the normal matrix inverse method to compute the inverse
solve(A)


#comptuing the inverse to return
cc <- makeCacheMatrix(A)

#printing the inverse computed
cacheSolve(cc)

# Checking if the function performs the same operation
identical(solve(A), cacheSolve(cc) )
