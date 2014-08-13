## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

	## initialize the value of the m (inverse matrix)
	m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
	## compute the inverse matrix using the 'solve' function
        setsolve <- function(solve) m <<- solve
	## get the value of the inverse matrix
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 	m <- x$getsolve()
	## if the inverse matrix is computed, it will not be calculated again
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
	## if the inverse matrix does not exist yet, it will be calculated
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
