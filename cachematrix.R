## Put comments here that give an overall description of what your
## functions do

# Matrix inversion is usually a costly computation and there may be some
# benefit to caching the inverse of a matrix rather than computing it
# repeatedly.

# makeCacheMatrix creates a list containing the following functions:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	# set the value of the matrix
	set <- function(y) {
		x <<- y
		inverse <<- NULL  # Initialize inverse to NULL everytime 
						  # a new matrix is set
	}
	# get the value of the matrix 
	get <- function() x
	# set the value of the inverse
	setinverse <- function(inv) inverse <<- inv
	# get the value of the inverse
	getinverse <- function() inverse
	list(set = set, get = get, 
		 setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if (!is.null(inv)) {  
		# when inv is present in the cache, use it and don't recompute.
		message("getting cached inverse")
		return (inv)
	}
	# Else compute the inverse and cache it.
	matr <- x$get()
	inv <- solve(matr, ...)
	x$setinverse(inv)
	inv
}
