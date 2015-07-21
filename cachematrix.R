## The below two functions can cache the inverse of a given matrix and return
## the cached value quickly thereby avoiding costly computational resources

## This function returns a special "matrix" object and a list of methods
## that can be applied to the object
makeCacheMatrix <- function(x = matrix()) {
    ## It makes use of <<- assignment operator to store values in cache to be 
    ## available to use outside of the function environment
    ## It takes a matrix as input and returns a list of 4 methods -namely set, 
    ## get, setinv, getinv that can be invoked for the special "matrix" object
    ## set function will initialize the variable in cache with the given matrix
    ## get function will retrieve the cached input matrix
    ## setinv function will initialize the variable in cache with the inverse
    ## getinv function will retrieve the inverse value of matrix from cache
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## This function returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    ## This function computes the inverse of the special "matrix" returned by 
    ## makeCacheMatrix function. If the inverse has already been calculated 
    ## (and the matrix has not changed), then the cachesolve should retrieve 
    ## the inverse from the cache
    ## Inverse of a square matrix can be computed using the solve function in R
    ## For example, if X is a square invertible matrix, then solve(X) returns 
    ## its inverse. A key assumption here is that matrix supplied is invertible
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
