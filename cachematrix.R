##My functions will create a special matrix that can cache its own inverse

## Make cache matix creates a special matrix object that stores the list to 
## set the value of the matrix, get the value of the matrix, set the matrix 
## inverse and get the matrix inverse 

makeCacheMatrix <- function(x = matrix()) {
  
  mat_inv<-NULL
  set<-function(y){
    x<<-y
    mat_inv<<-NULL
  }
  get<-function()x
  setinv<-function(inverse) mat_inv<<-inverse
  getinv<-function() mat_inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## This function calculates the inverse of the matrix created by the above 
## vector but first checks to see if the inverse has been calculated already
## If it has, it gets the value from the cache and skips the computation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mat_inv<-x$getinv()
  if(!is.null(mat_inv)){
    message("getting cached data")
    return(mat_inv)
  }
  data<-x$get()
  mat_inv<-solve(data, ...)
  x$setinv(mat_inv)
  mat_inv
}
