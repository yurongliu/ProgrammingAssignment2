## assignment 2 : caching the inverse of a matrix

## makeCacheMatrix creates a special "matrix", which is really a list containing function to
## 1.set the matrix
## 2.get the matrix
## 3.set the inverse matrix
## 4.get the inverse matrix
##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inver <- NULL
        set <- function(y){
		x <<- y
                inver <<- NULL
	}
        
	get <- function() x
	setinverse <- function(inverse) inver <<- inverse
	getinverse <- function() inver
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	inver <- x$getinverse()
        if(!is.null(inver)) {
                message("getting cached data")
                return(inver)
        }
        data <- x$get()
        inver <- solve(data, ...)
        x$setinverse(inver)
        inver
        
        ## Return a matrix that is the inverse of 'x'
}
