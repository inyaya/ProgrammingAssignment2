## This set of functions computes and caches the inverse of a matrix.

## makeCacheMatrix: Input should be an invertible matrix, 
    # output is a list of functions to cache and retrieve both a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {   
    #input x: matrix to work on
    
    # m contains the inverse matrix if it has been calculated already,
    # else it is set to NULL.
    m <- NULL
    
    # set a new input matrix and reset m
        # (makeCacheMatrix$set could be called for changing the input matrix)
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ### functions that will be called by cacheSolve:
    
    # return input matrix x to calling environment:
    get <- function() x
    
    # cache inverse matrix as m
    setInvMatrix <- function(invMatrix) m <<- invMatrix
    # return cached inverse matrix (or NULL)
    getInvMatrix <- function() m
    
    # return list of functions
    list(set = set, get = get,
         setInvMatrix = setInvMatrix,
         getInvMatrix = getInvMatrix)
}


## cacheSolve: only works in conjunction with makeCacheMatrix.
    # Input should be the vector of functions returned by makeCacheMatrix,
    # output is the reverse matrix.
    # If the reverse of the input matrix has already been calculated, then 
    # it is retrieved from makeCacheMatrix;
    # else the reverse is calculated using solve() and cached.

cacheSolve <- function(x, ...) {  
    # input x: vector of functions returned by makeCacheMatrix
    
    # get cached inverse matrix (or NULL if none exists)
    m <- x$getInvMatrix()
    
    # check whether cached InvMatrix exists - get if yes
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # the rows below will only be executed if no cached invMatrix exists
        # (because then return has not been called):
    
    # get the input matrix from cache using makeCacheMatrix$get
    data <- x$get()
    # calculate inverse matrix
    m <- solve(data)
    # call makeCacheMatrix$setInvMatrix to cache inverse
    x$setInvMatrix(m)
    
    # return inverse matrix
    m
    
}
