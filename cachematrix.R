## makeCacheMatrix returns a list which represents a special matrix object that can cache its inverse 
## cacheSolve return the inverse matrix of the special matrix returned by function makeCacheMatrix

## The returned list represents a matrix with "memory" capacity-its cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	  inverse = NULL
	  set<-function(y){
	  	 x<<-y
	  	 m<<-NULL
	   }
	   get<-function() x
	   setinverse<- function(matrix) m<<-matrix
	   getinverse<- function() m 
      list(set =set,get=get,setinverse = setinverse,getinverse=getinverse)
}


## This function returns the inverse matrix of x by first checking the cache value of x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        a = x$getinverse()
        if (!is.null(a)){
        	    message("get cached data")
        	    return (a)
        } 
        data= x$get()
        inversed = solve(data,...)
        x$setinverse(inversed)
		inversed
}
