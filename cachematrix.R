## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## use $set() function to set x before invoking cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## usage Notes:
## m1 <- makeCacheMatrix()
## m1$set(matrix(rnorm(9),3,3))
## cacheSolve(m1)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if (!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}