## R matrix function used to create matrix
makeCacheMatrix <- function(x = matrix()) {
	## initializing vector m by NULL
        m <- NULL
	## function set assigns value y to vector x and sets m to NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
	## function get retrieves vector x
        get <- function() x
	## function setmean stores value mean in m
        setmean <- function(mean) m <<- mean 
	## function getmean retrieves m
        getmean <- function() m
	## 
        list(set = set, 
		 get = get,
             setmean = setmean,
             getmean = getmean)
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmean()
	  ## if matrix exists then retrive it and end code execution
        if(!is.null(m)) {
		 ## if matrix is there, retrieve matrix
                message("getting cached data")
                return(m)
        }
	## if there is no matrix, get x and calculate matrix using solve function from R
        data <- x$get()
        m <- solve(data, ...)
	## store calculated matrix
        x$setmean(m)
	## return matrix to display
        m
}
