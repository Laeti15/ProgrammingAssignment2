## This second assignment takes advantage of the scoping rules (lexical scoping) 
## applied in R Programming to save computing time in situations where 
## potentially one needs to perform repeatedly the same calculation for 
## the same object. Here the calculation involves computing the inverse of 
## a matrix (using the solve function) with the assumption that the matrix is squared.
## Once the inversed matrix is calculated, reduction in computing time is attained 
## for subsequent inverse matrix calculation for the same input matrix since R 
## merely needs to look-up the cached inverse matrix (cached_inv). Cached values 
## are assigned temporarily to objects in an environment other than the current 
## environment using the <<- operator and originally set to NULL.

## First, a list of functions that can be referred to during the calculation is created. 
## This special object makeCacheMatrix includes a function to:
##     set the value of the matrix
##     get the value of the matrix
##     set the value of the inverse matrix
##     get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  cached_inv<-NULL
  set<-function(y){
    x<<-y
    cached_inv<<-NULL
  }
  get<-function()x
  setinvmtx<-function(invmtx) cached_inv<<-invmtx
  getinvmtx<-function() cached_inv
  list(set=set,get=get,setinvmtx=setinvmtx,getinvmtx=getinvmtx)
}


## Then, the second function gets the inverse matrix from the special matrix
## and checks if it is null or not. If it is not null, the console will print 
## a message advising it is getting the cached inverse matrix and it will 
## return such inverse. Here,  no additional computation is necessary.
## If is is null, it will get the value of the input matrix, calculate the inverse,
## set the inverse value in the cache function and return the result.

cacheSolve <- function(x, ...){
  cached_inv<-x$getinvmtx()
  if(!is.null(cached_inv)){
    message("Getting cached inverse matrix")
    return(cached_inv)
  }
  input_matrix<-x$get()
  cached_inv<-solve(input_matrix)
  x$setinvmtx(cached_inv)
  cached_inv
}
