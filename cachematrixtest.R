source("cacheMatrix.R")
cacheSolveTest <- function(){
  
  ## 3X3 matrix
  x <- matrix(c(1,1,1,0,2,2,1,2,1), 3,3)
  
  cacheX <- makeCacheMatrix(x)
  
  if(!identical(cacheX$get(),x))
    error("get() value is not same")
  
  inverseX <- cacheSolve(cacheX)
  identity3by3 <- diag(3)
  
  if( !identical(x %*% inverseX, identity3by3))
    error("inverseX * X is not resulting identity3by3")
  print("passed")
}

cacheSolveTest()