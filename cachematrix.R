## makeCacheMatrix function allows you to get and set a square matrix
## The makeCacheMatrix function creates a list containing a function to

##set the value of the matrix
##get the value of the matrix
##set the value of the inverse of the matrix
##get the value of the inverse of the matrix

## Usage:
## First instantiate the makeCacheMatrix and then use the set function to 
## set the value for the square matrix as:
## m1 <- makeCacheMatrix()
## m1$set(rnorm(9),3,3)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                if (class(y)!="matrix" ){
                        stop("Please set to a matrix.")
                }else if( nrow(y) != ncol(y) ){
                        stop("Please set to a Square Matrix.")
                }else if ( det(y) == 0 ){
                        stop("Cannot calculate inverse for Matrix with determinant Zero")
                }
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The cacheSolve function calculates the inverse of the matrix set by makeCacheMatrix function
## It calculate the invese if getInverse does not return anything otherwise it returns the cached 
## inversed of the matrix
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