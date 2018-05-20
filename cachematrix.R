## Put comments here that give an overall description of what your
## functions do
## The functions below calculates and returns the inverse of a given matix.

## Write a short comment describing this function
## makeCacheMatrix is a function containing the set, get, setinverse & getinverse functions.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve computes the inverse of the matrix returned by the makeCacheMatrix function. 
## If the inverse for this matrix has already been calculated, then the inverse will be retrived from the cache.
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}