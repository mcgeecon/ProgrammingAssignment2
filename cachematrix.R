##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(b = matrix()) {
    v <- NULL
    
    set <- function(a) {
        b <<- a
        v <<- NULL
    }
    
    get <- function() b
    setmatrix <- function(inverse) v <<- inverse
    getmatrix <- function() v
    list(set=set, get=get, setmatrix = setmatrix, getmatrix = getmatrix)
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed),
##then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(b, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    v <- b$getmatrix
    if(!is.null(v)) {
        message("getting cached data")
        return(v)
    }
    
    data <- b$get()
    v <- solve(data, ...)
    b$setmatrix(v)
    v
}

