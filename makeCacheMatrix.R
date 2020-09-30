makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  ## Set the value of the vector
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  ## Get the value of the vector
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}

## Testing the Programme
> source("makeCacheMatrix.R")
+ pmatrix <- makeCacheMatrix(matrix(1:16, nrow=4, ncol=4))
Error: unexpected symbol in:
"pmatrix <- makeCacheMatrix(matrix(1:16, nrow=4, ncol=4)
pmatrix"
> pmatrix <- makeCacheMatrix(matrix(1:16, nrow=4, ncol=4))
> pmatrix$get()
     [,1] [,2] [,3] [,4]
[1,]    1    5    9   13
[2,]    2    6   10   14

