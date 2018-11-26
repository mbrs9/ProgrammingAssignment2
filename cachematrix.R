## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function:
## makeCacheMatrix creates a matrix object; the set command 
## is used to assign values to the matrix "x" while the
## get command retrieves its values. 
## The inverse can be computed and stored via "setinv" and "getinv".

makeCacheMatrix <- function(x = matrix()) {
        a_inv <- NULL
        set <- function(y){
                x <<- y
                a_inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) a_inv <<- solve
        getinv <- function() a_inv
        list(set = set, get = get, 
             setinv = setinv, getinv = getinv)

}


## Write a short comment describing this function:
## cacheSolve retrieves the inverse of a matrix 
## returned by makeCacheMatrix and stored in the "cache".
## If the inverse has not been calculated, it computes 
## the inverse and stores it in the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        a_inv <- x$getinv()
        ## If the "x" matrix exists, then retrieves its inverse
        ## from the cache
        if (!is.null(a_inv)){
                message ("getting cached data (inverse)")
                return(a_inv)
        }
        ## If the inverse has not been calculated:
        ## Retrieves the original matrix,
        ## computes its inverse and stores it in the cache:
        data <- x$get()
        a_inv <- solve(data, ...)
        x$setinv(a_inv)
        a_inv
}
