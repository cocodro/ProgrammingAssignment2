## This function creates a special "matrix" object that can cache its inverse
## This functions creates the inverse matrix object, sets the new matrix object and
## corresponding NULL inverse matrix object.  It also creates the functions to get the matrix,
## as well as to get and set the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        invMat <- NULL
        set <- function(y) {
                x <<- y
                invMat <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) invMat <<- inverse
        getInverse <- function() invMat
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" object returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve
## should retrieve the inverse from the cache using the getInverse command from the makeCacheMatrix
## function.  If the inverse has not been solved and cached before, the function generates an inverse
## matrix object and sets it to the inverse matrix object in the first function.   After it finishes running, it
## returns the inverse of the supplied matrix.

cacheSolve <- function(x, ...) {
        invMat <- x$getInverse()
        if (!is.null(invMat)) {
                message("Retrieving cached data")
                return(invMat)
        }
        mat <- x$get()
        invMat <- solve(mat, ...)
        x$setInverse(invMat)
        invMat
}
