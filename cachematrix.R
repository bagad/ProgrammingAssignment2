## R programming peer graded assignment to calculate inverse of matrix and can cache the result
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## makeCacheMatrix function caches the inverse of matrix result


makeCacheMatrix<-function(x=matrix()){
  matrix_inv<-NULL
  
  ##set value of matrix
  set<-function(y){
    x<<-y
    matrix_inv<<-NULL
  }
  ##get value of matrix
  get<-function()x
  ## set inverse of matrix
  setinverse<-function(inv)matrix_inv<<-matrix_inv
  
  ##get inverse of matrix
  getinverse<-function()matrix_inv
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
cacheSolve<-function(x,...){
  
  matrix_inv <-x$getinverse()
  ## check if inverse of given matrix is already cached or not. if yes then return inverse from cache. 
  if(!is.null(matrix_inv)){
    return(matrix_inv)
  }
  ## if inverse of matrix is not cached then proceed to its calculation and cache the result
  data<-x$get() 
  ## calculate inverse of given matrix
  ## Computing the inverse of a square matrix can be done with the solve function.
  matrix_inv<-solve(data,...) 
  ##set the inverse of matrix or can say cache it
  x$setinverse(matrix_inv) 
  ## Return a matrix that is the inverse of 'x'
  matrix_inv 
}
