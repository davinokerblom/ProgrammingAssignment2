
## These two functions calculate the inverse of a given square invertible
## matrix and save the result in cache. If the inverse has already been 
## calculated and the matrix value has not changed it returns the cached 
## result and does not recalculate the inverse.

## This function creates a list of 4 functions which set/get the value of the 
## matrix and set/get the inverse of the matrix.

makecacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinv <- function(inv) m <<- inv
	getinv <- function () m
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function uses the getinv function from makecacheMatrix to check if
## the inverse has been calculated already(if so it returns that value).
## If getinv returns null the function calculates the inverse.

cachesolve <- function(x,...) {
	m <- x$getinv()
	if(!is.null(m)) {	
		print("getting cached data")
		return(m)
	} else {
		data <- x$get()
		m <- solve(data, ...)
		x$setinv(m)
		m
	}
}
