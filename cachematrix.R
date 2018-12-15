## Put comments here that give an overall description of what your
## functions do

#These funtions take a square matrix x as input and give you the inverse of that matrix.


## Write a short comment describing this function



makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL  #stores matrix in cache
  }
  get <- function() x  #Gets the matrix for you
  setInverse <- function(inverse) inv <<- inverse  #sets inverse matrix
  getInverse <- function() inv #gets inverse matrix
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse) #These lines create list of functions
}

## Write a short comment describing this function

##This function computes the inverse of the special matrix returned above. 
##If the matrix has stayed the same and  
## the inverse has already been calculated then it should retrieve the inverse from the cache


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
