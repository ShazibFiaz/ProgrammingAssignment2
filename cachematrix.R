## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL  ##creating a variable to store the inverse
  set<-function(y){
  x<<-y
  i<<-NULL
  }
  get<-function() x
  setinverse <- function(inverse) i <<- inverse ##setting the inverse to i
  getinverse <- function() i
  list(set = set, get = get, ## constructor to set and get the values
       setinverse= setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i<- x$getinverse() ##seeing if the inverse is cached or not 
  if(!is.null(i)) { ## if there is a cached matrix  the printing a message
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...) ## This will calcultae the inverse if the inverse is not cached
  x$setinverse(i)
  i
}
