## makeCacheMatrix(x)

## Takes a matrix (invertible), computes its inverse and caches the inversed matrix
## x - invertible matrix
## i - inversed matrix
## set, get - functions to save the query matrix into the cache, obtain the matrix from the cache
## setinverse, getinverse - functions to save the resultant inversed matrix into the cache, obtain the resultant inversed matrix

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
         getinverse = getinverse
         )
}

## cacheSolve(function)

## Takes the inverse matrix function and returns a cached copy of the inverse or
## if not cached, then computes the inverse, caches and returns the inverse
## x - makeCacheMatrix object
## i - inverse matrix
## data - invertible matrix

cachesolve <- function(x, ...) {
    i <- x$getinverse()
    if (!is.null(i)) {
        message("Get cached inverse")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
