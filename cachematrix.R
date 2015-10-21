## Get inverse of the matrix from cache  
## or calculate the inverse matrix and cache the result if it was not cached yet

## This function prepares a vector of functions to set/get matrix in/from cache, 
## and set/get inverse matrix in/from cache

makeCacheMatrix <- function(x=matrix()) {
  inv_mx<-matrix()
  
  set<-function(y) {
    x<<-y
    inv_mx<<-matrix()
  }
  
  get<-function() x
  
  setinverse<-function(new_inv_mx) {
    inv_mx<<-new_inv_mx
  } 
  
  getinverse<-function() {
    inv_mx
  }
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function checks if the inverse matrix for matrix in argument
## was already calculated and cached, it returnes the cached matrix, 
## or if the cached matrix only element is NA (matrix initialized as empty), 
## it calculates inverse matrix and caches the result then returns it

cacheSolve <- function(x,...) {

  res_mx<-x$getinverse()
  
  if((ncol(res_mx)>1)|(!is.na(res_mx[1,1]))) {
    message("Cached result returned")
    return(res_mx)
  }
    
    arg_mx<-x$get()
    res_mx<-solve(arg_mx)
    x$setinverse(res_mx)
      message("Calculated result returned")
    res_mx
}
