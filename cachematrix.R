## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The below function creates a special matrix object with 4 utility functions
## mset and iset set the matrix value and matrix inverse respectively
## mget and iget get the matrix value and matrix inverse respectively

makeCacheMatrix <- function(mat = matrix()) {

      ## mat is set to be of matrix() type

      inv<-NULL
      setmat<-function(mat2){
      ## sets value of matrix to mat2 passed in the argument
        mat <<- mat2
        inv <<- NULL
        }

      ## return value of the matrix
      getmat<-function(){mat}

      ## set inverse value for the matrix
      setinv<-function(inn){inv <<- inn}

      ## return inverse value stored in cache
      getinv<-function(){inv}

      ## return list of all functionalities
      list(mset=setmat,mget=getmat,iset=setinv,iget=getinv)

}


## Write a short comment describing this function

## Calculates inverse of the matrix set in the makeCacheMatrix object mat
## If inverse value already exists it is fetched from the cache and returned
## New value is calculated every time the matrix is changed
cacheSolve <- function(mat, ...) {
        ## Return a matrix that is the inverse of 'mat'
        inv<-mat$iget()
      	if(!is.null(inv)){
      		print("getting cache value")
      		return(inv)
      		}
        ## getting matrix value to solve for inverse
      	mtx<-mat$mget()
      	inv<-solve(mtx)
        ## setting new inverse value
      	mat$iset(inv)
      	inv
}
