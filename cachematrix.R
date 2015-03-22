## makeCacheMatrix demonstrates the concept of lexical scoping
## where environment with in which function is defined can be
## accessed using <<- operator from the environment when function is executed

## makeCacheMatrix creates essentially a list, with functions to 
## 1. set matrix
## 2. get matrix
## 3. set inverse of the matrix
## 4. get inverse of the matrix
## Note. Set function resets the inverse value

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inverse <<- i
  getinverse <- function() inverse
  list(get=get,set=set, setinverse=setinverse,getinverse =getinverse)
}


## cacheSolve calculates the inverse of the matrix, using R function solve
## argument x should be created using makeCacheMatrix 
## cacheSolve calls getinverse on cacheMatrix, and if it has a value, it returns the value
## otherwise, it calculates the inverse, sets the inverse on cachematrix before returning the value
## Note: It assumes x is always invertible
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i  
}

