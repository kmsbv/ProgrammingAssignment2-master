makeCacheMatrix <- function(x = matrix()) {
a<-NULL
  set<-function(y){
  x<<-y
  a<<-NULL
}
get<-function() x
setI<-function(solve) a<<- solve #calculate the inverse matrix
getI<-function() a
list(set=set, get=get,
   setI=setI,
   getI=getI)
}

cacheSolve <- function(x, ...) {
     a<-x$getI()
    if(!is.null(a)){
      message("getting cached inv matrix")
      return(a)
    }
    m<-x$get()
    a<-solve(m, ...)
    x$setI(a)
    a
}
##testing
x <-makeCacheMatrix(matrix(c(1,2,1,0,0,1,2,0,1),ncol=3,nrow=3))
cacheSolve(x)
x$get()
x$getI()
