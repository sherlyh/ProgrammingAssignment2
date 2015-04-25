## These functions are used to cache the inverse of a matrix

##Function makeCacheMatrix creates a special matrix that can cache its inverse. 
##The special matrix is a list containing functions to:
##1. set the value of matrix
##2. get the value of matrix
##3. set value of inverse matrix by using R function solve()
##4. get value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Function cacheSolve calculates the inverse of the special "matrix" created by makeCacheMatrix function with following steps:
##1. Check if inverse matrix has been created or not
##2. If yes, it get the inverse matrix from cache and stop the computation
##3. If not, it creates inverse matrix and set the value of matrix by using setinverse function (described in makeCacheMatrix)
##After computation, it will return the inverse of matrix 'x'
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
