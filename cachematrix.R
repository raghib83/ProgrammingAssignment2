## Put comments here that give an overall description of what your
## functions do

## This function will store the value of a specified matrix - setting its value, getting its value
## setting its inverse and getting its inverse. 

makeCacheMatrix <- function(x = matrix()) {
    InvMat <- NULL
    set <- function(y){
            x <<- y
            InvMat <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) InvMat <<- inv
    getInverse <- function() InvMat
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function will compute the inverse of the special "matrix" returned by the makeCacheMatrix.
## If the inverse has already been calculated, it will retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    InvMat <- x$getInverse()
    if(!is.null(InvMat)){
            message("getting cached data")
            return(InvMat)
    }
    data <- x$get()
    InvMat <- solve(data,...)
    x$setInverse(InvMat)
    InvMat
}
