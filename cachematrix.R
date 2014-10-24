## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {

        m <- NULL ##Originally define m
        set <- function(y) { ##set the value of the vector
            x <<- y  ##x<<- y is performed only when the function SET is called, 
                     ##and at that moment (and only then) x is overwritten by y.
                     ##y is the input you define when calling SET
            m <<- NULL ##Set the value of m, which is define in this environment 
        }              ##as NULL
        get <- function() {  ##get the value of the vector
            x
        }
        setinverse <- function(inve) { ##set the value of the invesre (as such - 
            m <<- inve                 ## "inve"). Here the inverse is not yet 
        }                              ##calculated. setmean is a function which
                                       ##allows you to set the inverse by hand.
                                       ## It is this function that is used by 
                                       ##cacheinverse to actually store the
                                       ##inverse in m.
        
        getinverse <- function() {  #get the value of the inverse
            m
        }
        list(set = set, get = get, ##handle the functions created by your call 
             setinverse = setinverse, ## to the makeCacheMatrix
             getinverse = getinverse)    
}


cacheSolve <- function(x, ...) { ##This function computes the inverse of the 
    ##special "matrix" returned by makeCacheMatrix above. If the inverse has 
    ##already been calculated (and the matrix has not changed), then  cacheSolve
    ##should retrieve the inverse from the cache.
    m <- x$getinverse()
    ## first checks to see if the mean has already been calculated. If so, it 
    ## gets the mean from the cache and skips the computation
    if(!is.null(m)) {
        ##every time a User calls $set, to set a different vector the m becomes
        ##NULL! so it doesn't return the cached value, but caches the new one
        message("getting cached data") 
        return(m)
    }
    ## Otherwise, it calculates the mean of the data and sets the value of the
    ## mean in the cache via the setmean function.
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}