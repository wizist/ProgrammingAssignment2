## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setInverse <- function(inv) i <<- inv
	getInverse <- function() i
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# This function computes the inverse of the special "matrix" returned by
# makeCacheMatrix above. If the inverse has already been calculated
# (and the matrix has not changed), then the cacheSolve should retrieve
# the inverse from the cache.
cacheSolve <- function(x, ...) {
	i <- x$getInverse()
	if (!is.null(i)) {
		message("Getting cached data")
		return(i)
	}
	m <- x$get()
	i <- solve(m, ...)
	x$setInverse(i)
	i
}
