## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  matinv<-NULL
  setmat<-function(y)
  {
    x<<-y
    matinv<<-NULL
  }
  getmat<-function() x
  setinv<-function(inverse) matinv<<-inverse
  getinv<- function() matinv
  list(setmat = setmat,getmat = getmat,setinv = setinv,getinv = getinv)
  

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matinv<-x$getinv()
  if(!is.null(matinv))
  {
    message("cached data is represented")
    return(matinv)
  }
  newdata<-x$getmat()
  matinv<-solve(newdata,...)
  x$setinv(matinv)
  matinv
}
