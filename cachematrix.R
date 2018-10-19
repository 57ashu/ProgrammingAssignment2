## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInverse <- function(solve) inv <<- solve
	getInverse <- function() inv
	list(set = set, get = get, 
	     setInverse = setInverse, 
	     getInverse = getInverse)
}

## This function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see 
## if the mean has already been calculated. If so, it gets the mean
## from the cache and skips the computation. Otherwise, it calculates
## the mean of the data and sets the value of the mean in the cache
## via the setInverse function.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
	if(!is.null(inv)) {
        message("getting cached data")
 		return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setInverse(inv)
	inv
}


## Sample run:
## > new <- matrix(c(1, 2, 3, 4), 2, 2)
## > m <- makeCacheMatrix(new)
## > m$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > cacheSolve(m)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > inv_new <- cacheSolve(m)
## getting cached data
## > inv_new
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > new %*% inv_new
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1
## >