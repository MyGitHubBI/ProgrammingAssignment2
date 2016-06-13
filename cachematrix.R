## Put comments here that give an overall description of what your
## functions do

## This function stores the data in cache

makeCacheMatrix <- function(x = matrix()) {
m<-NULL
set <-function(y = matrix()){ 
        x<<-y
        m<<-NULL
    }
get <- function() x
setinverse <- function(invrs) m <<- invrs
getinverse <- function () m
list(set = set, get = get, setinverse = setinverse, getinverse =getinverse)
}


## This functions looks in cache for inversed matrix, if not found, find a new matrix

cacheSolve <- function(x= matrix(), ...) {
  m<- x$getinverse()
  
  if(!is.null(m)){
    message("getting the cached data")
    return(m)
  }
  
  data<-x$get()
  m <-solve(data, ...)
  x$setinverse(m)
  
  
  ## Return a matrix that is the inverse of 'x'
  m
}
