## makeCacheMatrix defines an object which takes a matrix as input and 
## can store and return this matrix and its inverse matrix
## cacheSolve takes an object as defined by makeCacheMatrix as input and returns its inverse matrix
## The inverse matrix is calculated if NULL in the makeCacheMatrix object and later returned from the cache

## makeCacheMatrix takes a matrix as its input and returns a list containing 4 functions:
## set - initializes a makeCacheMatrix object by storing the matrix and setting the inverse to NULL
## get - returns the matrix stored
## setinverse - stores the inverse matrix when given as input
## getinverse - returns the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
            m <- NULL
            set <- function(y) {
                x <<- y
                m <<- NULL
            }
            get <- function() x
            setinverse <- function(inverse) m <<- inverse
            getinverse <- function() m
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
}


## cacheSolve takes a object as defined by makeCacheMatrix as input and returns its inverse matrix
## The inverse matrix is calculated if NULL in the makeCacheMatrix object and stored in the makeCacheMatrix object
## For subsequent function calls the inverse matrix is returned from the makeCacheMatrix object (cache)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            m <- x$getinverse()
            if(!is.null(m)) {
                message("getting cached data")
                return(m)
            }
            data <- x$get()
            m <- solve(data, ...)
            x$setinverse(m)
            m
}
