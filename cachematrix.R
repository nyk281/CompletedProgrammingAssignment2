
## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {  #creates a matrix
        
        m<- NULL	#sets m to NULL
        
        set <- function(y) {	# set function
                x <<- y
                m <<- NULL
        }
        get <- function() x	# get function that caches the inversed matrix
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Computs the inverse of the matrix returned by makecCacheMatrix function

cacheSolve <- function(x, ...) {
        
        m<-x$getsolve()  
        
        if (!is.null(m) {		# if statement to see if data is cached
                message ("getting cached data")
                return m
        }
        
        data<-x$get()	# function to invert the matrix if data is not cached
        m <- solve(data)
        x$setsolve(m)
        m
}