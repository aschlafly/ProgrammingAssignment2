## This combination of two functions will calculate the inverse of a matrix
## in a more efficient way by caching the results of calculations which allows
## them to be called later

## makeCacheMatrix creates a special matrix object that can then be passed to
## cacheSolve to either retrieve or calculate.
## cacheSolve will do the calculation after checking to see if the results
## are already stored in the parent environment.

makeCacheMatrix <- function(x = matrix()) {
    matinv <- NULL
    set <- function(m){
        x <<- m
        matinv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) matinv <<- solve
    getinv <- function() matinv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve takes as arguments the addresses in the parent envrionment
## of pre-calculated results. It will first check if the mean is stored in the
## addresses specified. 
## If so, it will simply read the results.
## If not, it performs the calculation and stores in the cache addresses. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    ## First check whether the address specified in the input vector already
    ## has the results of the pre-calculated vector
    matinv <- x$getinv()
    if(!is.null(matinv)){
        message("getting cached matrix inversion")
        return(matinv)
    }
    ## mat is the matrix stored in the address specified in the input vector
    mat <- x$get()
    matinv <- solve(mat)
    x$setinv(matinv) ## this line seems to have troubles
    matinv
}
