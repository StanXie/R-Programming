makeCacheMatrix <- function(x = matrix()) {
  cache<-NULL
  
  ##store a matrix
  set<-function(y){
    x<<-y
    cache<<-NUL
  }
  
  ##return to the stored matrix
  get<-function() x
  
  
  cacheInverse<-function(co) m<<-co
  getInverse<-function() m
  
  ##return to list
  list(set=set,
       get=get,
       cacheInverse=cacheInverse,
       getInverse=getInverse)
}

cacheSolve<-function(x,...){
  ##get the cached value
  m<-x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-co(data,...)
  x$cacheInverse(m)
  m
}
