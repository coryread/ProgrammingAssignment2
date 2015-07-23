## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##function to cache a matrix object that can cache its inverse
##at the end of the program I saved my testing scripts

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL ##initialize m to null or empty
  set <- function(y) {
    x <<- y ## give x the value y in a cached env
    m <<- NULL ##give m a null value in the cache env
  }


    get <- function() return(x) ##
    setinv <- function(inv) m <<- inv
    getinv <-function() return(m)
      return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## This function computes the invers of the special 
## matrix returned by the function "makeCacheMatrix".  if the inverse
## has already been computed and has not changed, then 
##the following function "cacheSolve" should retrieve the inverse from the 
## cache and not waste time calculating the inverse

cacheSolve <- function(x, ...) {
        
  m <- x$getinv()
  if(!is.null(m)) {
      message("getting cached data")
      return(m)
  }
  
  data <-x$get() 
  m <- solve(data, ...)
  x$setinv(m)
  return(m)
}
###testing scripts example from forums (Paolo Di Lorenzo)
##    mat <- matrix(c(1,0,5,2,1,6,3,4,0),nrow = 3,ncol = 3)
##    print(mat)

##    matrixx <- makeCacheMatrix(mat)

##    matrixx$get()

##    matrixx$getinv()

##    cacheSolve(matrixx)

##    cacheSolve(matrixx)


