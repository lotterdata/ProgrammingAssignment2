## These functions create matrix objects that can cache their inverses
## so that inverse can be retreived without recalculating

## makeCacheMatrix creates a matrix along with the functions needed
## to cache the inverse

makeCacheMatrix <- function(x = matrix()) {
    minv <- NULL
    set <- function(y) {
        x <<- y
        minv <<- NULL
    }
    get <- function() x
    set.inverse <- function(i) minv <<- i
    get.inverse <- function() minv
    list(set = set, get = get, 
         set.inverse = set.inverse, 
         get.inverse = get.inverse)
}


## cacheSolve returns the inverse of x, calculating the inverse only if
## necessary, otherwise returning a cached value

cacheSolve <- function(x, ...) {
    minv <- x$get.inverse()
    if (!is.null(minv)) { #check if inverse is cached
        message("getting cached value")
        return(minv)
    }
    data <- x$get() #not cached, so calculate
    minv <- solve(data,...)
    x$set.inverse(minv) #cache the inverse just calculated
    minv
}
