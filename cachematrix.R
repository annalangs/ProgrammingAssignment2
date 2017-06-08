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
	r <- NULL
	set <- function (y) {
			x <<- y
			r <<- NULL
	}
	get <- function () x
	setreverse <- function (reversematrix) r <<- reversematrix
	getreverse <- function() r
	# return the list of four functions that cache the value of reverse matrix
	list (set=set, get=get, setreverse=setreverse, getreverse=getreverse)
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
        r <- x$getreverse()
        if (! is.null(r)) {
        	message("getting cached data")
        	return (r)
        	}
        data <- x$get()
         r <- solve(data, ...)
         x$setreverse(r)
         r
}
