
## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	i<-NULL
	## seti 

	## function to set the matrix
	seti <- function(y){
		x<<-y
		i<-NULL
	}

	## function to get the matrix
	geti <- function()x

	## function to set the inverse of the matrix once it is calculated
	setinv <- function(i1) i<<-i1

	## function to get the cached inverse
	getinv <- function() i
	list(seti=seti, geti=geti,setinv=setinv,getinv=getinv)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix 

cacheSolve <- function(x, ...) {
        
	i <- x$getinv()
	if(!is.null(i)){
		message("getting cached inverse")
		return(i)
	}

	## assign the matrix for which inverse is to be calculated
	data <- x$geti()
	
	## calculate inverse of the matrix
	i <- solve(data,...)

	## cache the inverse calculated
	x$setinv(i)

	## Return a matrix that is the inverse of 'x'
	i
}
