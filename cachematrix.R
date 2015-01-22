# This is a cached version of the Matrix inversion function.
# The idea of caching is used to reduce the computation time required 
# for finding the inverse of a matrix when it is called repeatedly.
# The following two functions are used to cache the inverse of a matrix.
# Usage examples:
# > a <- makeCacheMatrix()
# > a$set( matrix(1:4, 2, 2))
# > cacheSolve(a)
# >      [,1] [,2]
# > [1,]  -2   1.5
# > [2,]   1  -0.5
# >
# Another Usage Example
# > b <- makeCacheMatrix(matrix(1:4,2,2))
# > cacheSolve(b)
# >      [,1] [,2]
# > [1,]  -2   1.5
# > [2,]   1  -0.5
# >



makeCacheMatrix <- function(x = matrix()) {
    # this function returns a list of functions to
    # 1. set the value of the matrix
    # 2. get the value of the matrix
    # 3. set the value of inverse of the matrix
    # 4. get the value of inverse of the matrix
    
    cache <- NULL
    set <- function(y) {
        x <<- y
        cache <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) cache <<- inv       
    getInverse <- function() cache         
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# The cacheSolve function returns the inverse of the matrix. 
# It first checks if the inverse is already in the cache. If so, it returns it otherwise,
# it computes the inverse and save this in the cache using  setinverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
	    message("getting cached data")
	    return(inv)
    }
    matx <- x$get()
    inv <- solve(matx)
    x$setInverse(inv)
    inv
}



