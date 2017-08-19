## The functions below can be used to create a special "matrix" object that can 
## cache the inverse of a matrix rather than compute it repeatedly 


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    InverseMatrix <- NULL
    
    # store the new matrix in cache (x) and delete the old inverse matrix
    set <- function(y) {
        x <<- y
        InverseMatrix <<- NULL
    }
    # first (get): get the matrix in cache
    # second (setInverseMatrix): store the inverse matrix in cache
    # third (getInverseMatrix): get the cached inverse matrix
    # fourth (list): store the four functions in a list
    get <- function() x
    setInverseMatrix <- function(solve) InverseMatrix <<- solve
    getInverseMatrix <- function() InverseMatrix
    list(set = set,
         get = get,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    # get the inverse matrix of x
    InverseMatrix <- x$getInverseMatrix()
    
    # check whether there is a cached matrix available
    if(!is.null(InverseMatrix)) {
            message("getting cached matrix")
            return(InverseMatrix)
    }
    
    # if there is no cached matrix available, here we create the inverse matrix with the solve function
    # and set the new inverse matrix in cache
    data <- x$get()
    InverseMatrix <- solve(data, ...)
    x$setInverseMatrix(InverseMatrix)
    InverseMatrix
}