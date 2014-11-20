## Used together, the following two functions return the inverse of a matrix.
## First, pass the matrix to be inverted as an argument to makeCacheMatrix and
## store the result in an object. Then, pass the object as an argument to cacheSolve.
## Before calculating the inverted matrix, cacheSolve checks to see whether the
##  inverted matrix has already been cached, thus saving precious hours of processing.


## makeCacheMatrix takes an invertible matrix as an argument and returns a list
## containing three functions: get, setinv, and getinv. These functions are
## later used by cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	get <- function() x
	setinv <- function(thing) inv <<- thing
	getinv <- function() inv
	list(get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve takes the result of makeCacheMatrix as an argument and tests whether
## the inverted matrix has already been cached. If so, the function returns the
## inverted matrix from the cache. If not, the function calculates and returns
## the inverted matrix.

cacheSolve <- function(x, ...) {
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("Getting cached data...")
		return(inv)
	}
	dat <- x$get()
	inv <- solve(dat)
	x$setinv(inv)
	inv
}