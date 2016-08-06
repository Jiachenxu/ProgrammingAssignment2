## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a 
## matrix rather than compute it repeatedly (there are also 
## alternatives to matrix inversion that we will not discuss
## here). Your assignment is to write a pair of functions 
## that cache the inverse of a matrix.

## In this assignment, the function could finish the following things:
## 1.set the value of the vector
## 2.get the value of the vector
## 3.set the value of the mean
## 4.get the value of the mean

## makeCacheMatrix: This function creates a special "matrix" 
## object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the
## special "matrix" returned by makeCacheMatrix above. If
## the inverse has already been calculated (and the matrix
## has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  matrix1 <- x$get()
  i <- solve(matrix1, ...)
  x$setinverse(i)
  i
}





> ## an example for this two function
> a=makeCacheMatrix(matrix(c(60,0,0,0,90,0,0,0,150),3,3))
> a$get()
     [,1] [,2] [,3]
[1,]   60    0    0
[2,]    0   90    0
[3,]    0    0  150
> a$getinverse()
NULL
> cacheSolve(a)
           [,1]       [,2]        [,3]
[1,] 0.01666667 0.00000000 0.000000000
[2,] 0.00000000 0.01111111 0.000000000
[3,] 0.00000000 0.00000000 0.006666667
> cacheSolve(a)
getting cached data
           [,1]       [,2]        [,3]
[1,] 0.01666667 0.00000000 0.000000000
[2,] 0.00000000 0.01111111 0.000000000
[3,] 0.00000000 0.00000000 0.006666667
