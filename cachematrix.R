## the function takes a matrix 
## and creates an inverse of the matrix using the solve function

## vac stands for vacant aka NULL

makeCacheMatrix <- function(x = matrix()) 
{
  vac <- NULL
  set<-function(y){
    y <<- x 
    vac <<- NULL
  }
  get<-function() x
  setinverse<-function(inv) vac <<- inv
  getinverse<-function() vac
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## the function takes a matrix 
## and creates an inverse of the matrix using the solve function
## vac stands for vacant aka null

cacheSolve <- function(x, ...) {
  vac <- x$getinverse()
  if(!is.null(vac)) {
    message("getting cached inverse matrix")
    return(vac)
  }
  data <- x$get()
  vac <- solve(data, ...)
  x$setinverse(vac)
  vac
}
