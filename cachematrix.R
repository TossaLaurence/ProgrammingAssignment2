
## functions do


makeCacheMatrix <- function(x = matrix()) {
  inv_mat <- NULL
  ##creation of matrix x
  set <- function(y) {
    x <<- y
    inv_mat <<- NULL
  }
  setInv <- function() {
    ##inversion of a inversible matrix
    if(ncol(x)==nrow(x)) {inv_mat <<-solve(x)} 
    else{inv_mat<- NULL}
  }
  get <- function() x
  
  getInv<-function() inv_mat
  
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)

}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv_mat <- x$getInv()
  if(!is.null(inv_mat) && data == x$get()) {
    message("getting cached data")
    return(inv_mat)
  }
  data <- x$get()
  inv_mat <- x$setInv()
  inv_mat
  
}
