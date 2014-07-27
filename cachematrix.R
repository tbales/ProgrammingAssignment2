## Cache the Inverse of a Matrix

## Create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        # set inverse variable
        m <- NULL
        
        # set the matrix
        set <- function(matrix) {
                x <<- matrix
                m <<- NULL
        }
        
        # get the matrix 
        get <- function() {
        	x
        }
        
        # set the inverse of the matrix
        set_inverse <- function(inverse) {
        	m <<- inverse
        }	
        
        # get the inserve of the matrix
        get_inverse <- function() {
        	m
        }
        
        # list internal functions
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)

}


## Compute the inverse of the special "matrix" returned by makeCacheMatrix()
## If the inverse has already been calculated (and the matrix has not changed),
## the retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        m <- x$get_inverse()
	
	# or else, retrieve the inverse matrix from the cache
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	
	# get the original matrix
	data <- x$get()
	
	# calculate the inverse of the matrix using matrix multiplication
	m <- solve(data %*% data)
	
	# assign the inverse to x
	x$set_inverse(m)
        
        # return the matrix
        m
}
