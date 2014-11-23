##  The code of class.coursera.org/rprog-009 peer assignment "Programming Assignment 2: Lexical Scoping".
##  Introduce the <<- operator to assign a value to an object in an environment that is different from the current environment. 

##  Below are two functions that are used to create a special object that stores a numeric matrix and cache's its inverse.

##  The function makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##     set the value of the matrix
##     get the value of the matrix
##     set the matrix inverse
##     get the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    invs <- NULL
    set <- function(y) {
        x <<- y
        invs <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invs <<- inverse
    getinverse <- function() invs
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


##  The function cacheSolve solve the inverse of the special "matrix" created with the function makeCacheMatrix.
##  It first checks the inverse is already existed. If so, it gets the inverse from the cache and skips the computation. 
##  Otherwise, it solve the inverse of the data and sets the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invs <- x$getinverse()
    if(!is.null(invs)) {
        message("getting cached data")
        return(invs)
    }
    data <- x$get()
    invs <- solve(data, ...)
    x$setinverse(invs)
    invs
}
