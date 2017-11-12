## The makeCacheMatrix and cacheSolve function together are used to avoid computing the inverse of a matrixfunction
## if it has already been computed and cached. Computing the inverse of a matrix is computationally expensive so it 
## is advantageous to retrieve the inverse from the cache if it exists

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {

    i<-NULL
    set<-function(y) {
        x<<-y
        i<<-NULL
    }
	
    get<-function()x
    
	setInverse<-function(inv) i <<-inv
    
	getInverse<-function() i
    
	list (set=set, get= get, 
          setInverse=setInverse, 
          getInverse = getInverse)
}



## The following function returns the inverse of the matrix. It first checks if
## the inverse has already been computed. If so, it gets the result and skips the
## computation. If not, it computes the inverse, and caches it. Not all matrices are 
## invertible; however, it is assumed that only invertible functions will be passed to this function
## 

cacheSolve <- function(x, ...) {
   
	i<-x$getInverse()
   
   if(!is.null(i)) {
        message("Retrieving cached inverse")
        return(i)
    }

    data<-x$get()

    i<-solve(data)

    x$setInverse(i)

    i
		
}
