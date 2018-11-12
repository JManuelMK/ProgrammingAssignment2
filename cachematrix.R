## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This following function creates a special object, a list that:
##          set the value of the matrix
##          get the value of the matrix
##          set the inverse of the matrix
##          get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      m<-NULL
      set<-function(y){
            x<<-y
            m<<-NULL
      }
      get<-function() x
      setinverse<-function(solve) m <<-solve
      getinverse<-function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
      }


## Write a short comment describing this function
## The following function caclualtes the inverse of the special object (matrix) calculated in the
## above function.It checks of the inverse has been already calculated and if it is the case it just
## retrieves it skipping the computation.

cacheSolve <- function(x, ...) {
      m<-x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      ## Return a matrix that is the inverse of 'x'
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m 
}

#e.g.
m<-matrix(c(2,4,1,5),nrow=2,ncol=2)
test<-makeCacheMatrix(m)
cacheSolve(test)
