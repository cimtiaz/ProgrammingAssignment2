## This program calculate the inverse of a square matrix
## It contains two functions makeCacheMatrix, and cacheSolve

## The function makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

	matinverse <- NULL
	
	set <- function(y) {
		x <<- y
		matinverse <<- NULL
	}
	
	get <- function() {x}
	
	setinv <- function(tempinverse) {matinverse <<- tempinverse} 
	
	getinv <- function() {matinverse}
	
	list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## The function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		matinverse <- x$getinv()

        if (!is.null(matinverse)){
            message("Matrix data not changed, so getting data from cache")
            return(matinverse)
        }
        
        matdata <- x$get()
        matinverse <- solve(matdata, ...)
        
        x$setinv(matinverse)
        
        return(matinverse)
}
