## Assignment

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    theMatrix <- NULL
    
    set <- function(y){
        x <<- y
        theMatrix <<- NULL
    }
    
    get <- function() x
    
    setInvmatrix <- function(solve) theMatrix <<- solve
    
    getInvmatrix <- function() theMatrix
    list(set = set, get = get,
         setInvmatrix = setInvmatrix,
         getInvmatrix = getInvmatrix)
}

## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inverted <- x$getInvmatrix()
    if(!is.null(inverted)){
        ## then we have cached data
        print("cached")
        return(inverted)
    } else {
        ## not cached
        tempMatrix <- x$get()    ##get the matrix
        ## print("debug...")
        ## print(tempMatrix)
        inverted <- solve(tempMatrix, ...)  ##invert matrix
        x$setInvmatrix(inverted)               ##store in cache
        print("not cahced")
        inverted
    }
}
