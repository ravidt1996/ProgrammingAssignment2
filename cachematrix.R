
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## here the function is taking input of a matrix which is cached
  matinv<-NULL ## Intialising Matinv to be null, it will get value of matrix inverse
  setmat<-function(y) ## Set function to assign if any new
  {
    x<<-y  ## assigning value of matrix in parent environment
    matinv<<-NULL ##if there is new matrix set matinv to null 
  }
  getmat<-function() x ## returns value of matrix argument
  setinv<-function(inverse) matinv<<-inverse ##assigns value of matinv in parent environment
  getinv<- function() matinv ## returns inverse of matrix
  list(setmat = setmat,getmat = getmat,setinv = setinv,getinv = getinv) 
  ## The above line is used it inorder to call them by $ operator
  

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ##If the  matrix matrix is already present it will repesent the cached matrix inverse
  matinv<-x$getinv()
  if(!is.null(matinv)) ## checks if the matinv is null or not  
  {
    message("cached data is represented") # if it is not null represents cached data 
    return(matinv) # returning matinv of cached data
  }
  newdata<-x$getmat() # Fed as new data
  matinv<-solve(newdata,...) # solve function is used for inverse calcalution
  x$setinv(matinv) # setting the new matrix inverse
  matinv # returning matrix inverse
}
