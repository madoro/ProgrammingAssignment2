## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#The first function, makeCacheMatrix creates a special "matrix", object that can cache its inverse. 
# But it is really a list containing a function to:

# 1.set the value of the matrix
# 2.get the value of the matrix
# 3.set the value of the matrix inverse
# 4.get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}




## Write a short comment describing this function
# The following function calculates the matrix inverse of the special "matrix" created with 
# the above function. However, it first checks to see if the matrix inverse has already been 
# calculated. If so, it gets the matrix inverse from the cache and skips the computation. 
# Otherwise, it calculates the matrix inverse of the data and sets the value of the mean 
# in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse() # get the data 
  if(!is.null(m)) {  # evaluate if it has changed
    message("getting cached data")
    return(m)  
  }
  data <- x$get() # get the new data
  m <- solve(data, ...) # calculate the matrix inverse
  x$setinverse(m) # set the new inverse matrix
  m
  
}
