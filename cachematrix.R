## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	## 1. set the value of the matrix
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	## 2. get the value of the matrix
	get <- function() x
	## 3. set the value of the inverse of the matrix
	setInverse <- function(solve) inv <<- solve
	## 4. get the value of the inverse of the matrix
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
    ## Get inverse from the cache
    inv <- x$getInverse()
    ## Check if the inverse exists in the cache, and return it if it
    ## does
	if(!is.null(inv)) {
        message("getting cached data")
 		return(inv)
	}
    ## In case the inverse is not in the cache, calculate it
	data <- x$get()
	inv <- solve(data)
	## Set inverse in cache
	x$setInverse(inv)
	## Return the inverse
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