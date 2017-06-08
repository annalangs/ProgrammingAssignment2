## Functions that compute and cache the inverse matrix
## function makeCacheMatrix makes a cached matrix out of argumet x
## function cacheSolve takes in an argument from makeCacheMatrix
# and returns a reverse matrix for x. If x is not square or not invertible
## and arror is returned. 
## Sample use: 	X<-matrix( c(1, 2, 3, 4), nrow=2, ncol=2)
##   			cachedX <- makeCacheMatrix(X)
##				cacheSolve(cachedX)
##				cacheSolve(cachedX)

## makeCacheMatrix creates and returns a list of functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function (y) {
			x <<- y
			inv <<- NULL
	}
	get <- function () x
	setInverse <- function (invmatrix) inv <<- invmatrix
	getInverse <- function() inv
	# return the list of four functions that cache the value of inverse matrix
	list (set=set, get=get, 
	setInverse=setInverse, getInverse=getInverse)
}

## Argument x is the list of functions returned by makeCacheMatrix
## CacheSolve calculates the reverse of the cacheMatrix create by makeCacheMatrix
## It first checks if the reverse has already been calculated, 
## if so it gets the reverse from the cache and skips the computation. 
## Otherwise, the reverse is calculated and cached.
## cacheSolve uses computes the inverse matrix usingsolve function.
## It assumes that matrix is square and invetible.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invmatrix <- x$getInverse()
        if (! is.null(invmatrix)) {
        	message("getting cached data")
        	return (invmatrix)
        	}
         matrix <- x$get()
         invmatrix <- solve(matrix, ...)
         x$setInverse(invmatrix)
         invmatrix
}
