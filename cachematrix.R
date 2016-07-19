## The functions below will create a special matrix that will allow us to 
## cache its inverse and then either retrieve the cached inverse or compute
## the inverse of a new matrix and cache it. Here goes nothing!

## This function creates the special "matrix", a list of functions that
## allow us to get and set the matrix and its inverse in the cache.

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      mk_Matrix <- function(y) {
            x <<- y
            i <<- NULL
      }
      get_Matrix<-function() x
      invert_Matrix<-function(solve) i<<-solve
      get_invert<-function() i
      list(mk_Matrix=mk_Matrix,
           get_Matrix=get_Matrix,
           invert_Matrix=invert_Matrix,
           get_invert=get_invert)
}


## This function computes the inverse of a matrix of type makeCacheMatrix().
## If the inverse is already stored in the cache, that value is printed to the
## screen. Otherwise, the inverse is calculated, sent to the cache, and printed.

cacheSolve <- function(x, ...) {
      i<-x$get_invert()
      if(!is.null(i)){
            message("retrieving cached inverted matrix")
            return(i)
      }
      mat<-x$get_Matrix()
      i<-solve(mat,...)
      x$invert_Matrix(i)
      i
}
