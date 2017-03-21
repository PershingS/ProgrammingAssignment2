
# The two functions below follow the vector example that does the 
# same thing except it takes in matrix (not a vector) and finds it's inverse (not the mean of
# of the vector). 

# The first function takes in a matrix or sets up a default null matrix in order as a filler
# and also sets up the needed functions that are only available for the cacheable matrix.
# Those functions can then be used within the second function which, will solve and find the
# inverse of the cached matrix.  HOWEVER, it is possible to bypass the second 
# function entirely by just using the functions attached to the invertable, cached matrix. 



makeCacheMatrix <- function(x = matrix()) {
    im<-NULL                            
    set<-function(y){                    
    	    x<<-y                             
    	        im<<-NULL                         
    	           }
  get<-function() x                       
  setinverse<-function(solve) im<<- solve 
  getinverse<-function() im               
    list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}

}


# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
# retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
       im<-x$getinverse()                   
  if(!is.null(im)){                    
    message("getting cached data")       
    return(im)                         
  }
  matrix<-x$get()                     
  im<-solve(matrix, ...)               
  x$setinverse(im)                     
  im                                   
}


