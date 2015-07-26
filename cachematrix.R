## These functions enable cacheing of potentially time-consuming computations to return the inverse
## of a matrix where on the first occasion the functions are run the computation takes place.
## On subsequent occasions the inverse is retrieved from the cache.

## makeCacheMatrix sets up a list of 4 functions: to get and set the matrix, and to get and
## set the inverse. 
## example input:
## source(makeCacheMatrix)
## create the set of 4 functions and set up the matrix:
## a <- makeCacheMatrix(x=matrix(sample(1:25),5,5))

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) inv <<- solve
    print(solve)
    getsolve <- function() inv
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## source(cacheSolve)
## cacheSolve(a)
## If the inverse is NULL (not previously computed) then this function calculates 
## the inverse using inputs "a". If previously calcuated then the function uses what
## is stored in "inv" and reports that stored data is being used.

cacheSolve <- function(x, ...) {
    inv <- x$getsolve()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data,...)
    x$setsolve(inv)
    inv
        ## Return a matrix that is the inverse of 'x'
}
