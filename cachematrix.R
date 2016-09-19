## cachematrix.R 
## 
## Above functions aims at  providing inverse of a matrix
## and prevent unnecessary calculation by caching the result
## each time a matrix is solved.

## this function will create the matrix object and will cache 
## its value upon calculation

makeCacheMatrix <- function(x = matrix()) {
        mxInv <- NULL
        set <- function(y){
                x <<- y
                mxInv <<- NULL
        }
        get <- function() x
        setInv <- function(solve) mxInv <<- solve
        getInv <- function() mxInv
        list(set = set, get = get, 
             setInv = setInv, 
             getInv = getInv)
}


## this function ask the cache for an already inverted 
## matrix object, built with makeCacheMatrix(). 
## If not available, it call the setter which
## will put the result in cache

cacheSolve <- function(x, ...) {
        mxInv <- x$getInv()
        if (!is.null(mxInv)){
                message("getting cached data")
                return(mxInv)
        }
        data <- x$get()
        mxInv <- solve(data,...)
        x$setInv(mxInv)
        mxInv
}
