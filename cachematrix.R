## Functions that cache the inverse of a matrix
## this function creates a spacial matrix that caches its inverse 

makeCacheMatrix <- function(x = matrix()) {
    inver <- NULL
    set <- function(y){
        x <<- y
        inver <<- NULL
    }
    get <- function() x
    setInverse <- function(solveMatrix) inver <<- solveMatrix
    getInverse <- function() inver
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## this function will compute the inverse of the spacial matrix that is returned by the makeCacheMatrix function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inver <- x$getInverse()
    if(!is.null(inver)){
        message("getting cached data")
        return(inver)
    }
    data <- x$get()
    inver <- solve(data)
    x$setInverse(inver)
    inver
}
