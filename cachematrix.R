## Compute the inverse of a squar matrix
## compute the inverse if not already computed and store the result
## if the inverse is already computed, then just return the cached value

## Store the input matrix and setup the caching functions

makeCacheMatrix <- function(x = matrix()) {
    	inverse <- NULL
    	set <- function(y) {
    		x <<- y
    		inserve <<- NULL
    	}
    	get <- function() x
    	setinverse <- function(inv) inverse <<- inv
    	getinverse <- function() inverse
    	list(set = set, get = get,
    	     setinverse = setinverse,
    	     getinverse = getinverse)
}


## compute the inverse (or return previously computed cached value)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if  (!is.null(inverse)) {
          message("getting cached data")
          return inverse
        }
        data <- x$get()
        inverse <- solve(data)
        x$setinverse(inverse)
        inverse
}

