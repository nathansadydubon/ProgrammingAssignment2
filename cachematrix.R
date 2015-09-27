## These two functions cache an inverse of a matrix
## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly.
## Below are the two functions that create a special object that stores 
## a matrix and then caches its inverse.



## makeCacheMatrix : This function creates a special "matrix" object
## that can cache its inverse.
## it is really a list containing functions to
##
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    ## initialize the cache value "m"
    m <- NULL
    ## function "set" assigns a matrix to the internal variable "x" and clears cache "m"
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## function "get" retreives the internal matrix "x"
    get <- function() x
    ##function to set the inverse to the cache "m"
    setinverse <- function(inv_m) m <<- inv_m
    ##function to get the cached inverse matrix stored in "m"
    getinverse <- function() m
    ##list all of the values in makeCachMatrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.
## it first checks to see if the inverse has already been calculated. 
## If so, it get`s the inverse from the cache and skips the computation. 
## Otherwise, it calculates the mean of the data and sets the value of the 
## mean in the cache via the `setinverse` function.

cacheSolve <- function(x, ...) {
    ##set the value of "m" to the inverse of the special matrix object
    m <- x$getinverse()
    ##test to see if the inverse has already been computed
    if(!is.null(m)) {
        ## if the inverse is already cached, print out a message and return the inverse
        message("getting cached data")
        return(m)
    }
    ##of the value of "m" was null, then the matrix inverse does not exist, so solve for it
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    ## Return a matrix that is the inverse of 'x'
    m
}
