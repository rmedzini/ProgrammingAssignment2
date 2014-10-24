## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL #Originally define m
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        get <- function() {
            x
        }
        setinverst <- function(inve) { 
            m <<- inve
        }
        getinverst <- function() {
            m
        }
        list(set = set, get = get,
             setinverst = setinverst,
             getinverst = getinverst)    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverst()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverst(m)
        m
}
