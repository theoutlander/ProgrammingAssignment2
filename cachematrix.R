##
## The makeCacheMatrix function creates a special "matrix" object 
## that can cache its inverse.
##
## This cacheSolve function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), then 
## cacheSolve retrieves the inverse from the cache.
##

## This function takes in a parameter as a matrix and stores it internally
## It provides setters and getters to the matrix as well as the inverse 
## of the matrix
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

## This function takes in a matrix as a parameter and returns
## the inverse of the parameter (matrix). It also utilizes the cache to store results
## and serves the results from the cache if it exists
cacheSolve <- function(x, ...) {
    
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