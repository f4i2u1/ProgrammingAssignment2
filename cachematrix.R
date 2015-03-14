## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly. The two 
## functions below will help to achieve this caching technique.
##
## Function makeCacheMatrix creates a special matrix, which is really a list 
## containing:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix
##
## Function cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

## This function create a special matrix containing a list of
## 'getters' and 'setters' as explained above for the purpose of 
## caching the inverse of a matrix
## Args: 
##      x: The matrix to be computed for the inverse
## Returns: 
##      a list of setters and getters
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve<- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
    
}


## This function compute the inverse of a special matrix x
## created using makeCacheMatrix function above
## Args: 
##      x: a special matrix created using makeCacheMatrix
## Returns: 
##      an inverse of the special matrix x
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
