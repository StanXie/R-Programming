makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NUL
  }
  get<-function() x
  setmaen<-function(mean) m<<-mean
  getmean<-function() m
  list(set=set,
       get=get,
       setmean=setmean
       getmean=getmean)
}

cachemean<-function(x,...){
  m<-x$getmean()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-mean(data,...)
  x$setmean(m)
  m
}