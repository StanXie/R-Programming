##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  
  ##store a matrix
  set<-function(y){
    x<<-y
    inv<<-NUL
  }
  
  ##return to the stored matrix
  get<-function() x
  setinverse <- function(inv) inv <<- inv
  getinverse <- function() inv
  
  ##return to list
  list(set = set, get = get,  setinverse = setinverse, getinverse = getinverse)
}

##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve<-function(x,...){
  ## Return a matrix that is the inverse of 'x'
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinv(inv)
  inv
}
