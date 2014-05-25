## makeCacheMatrix(x)

## Takes a matrix (invertible), computes its inverse and caches the inversed matrix
## x - invertible matrix
## i - inversed matrix
## set, get - functions to save the query matrix into the cache, obtain the matrix from the cache
## setinverse, getinverse - functions to save the resultant inversed matrix into the cache, obtain the resultant inversed matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL                           # Container for the cached inverted matrix
    set <- function(y) {                
        x <<- y                         # Store the input matrix in memory, for get() to get
        i <<- NULL                      # Reset the output container
    }

    get <- function() x                 # Get the original invertible matrix, makeCacheMatrix was called with
    setinverse <- function(solve) i <<- solve # Set the scope for the container
    getinverse <- function() i                # Get the inversed matrix from the cache
    list(set = set, get = get,                # Symbols/Args
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
    i <- x$getinverse()                 # Load the inversed matrix stored in the cache
    if (!is.null(i)) {                  # Test if an inversed matrix exists
        message("Get cached inverse")
        return(i)                       # Send the result and exit
    }
    data <- x$get()                     # If the inversed matrix doesn't exist yet, load the invertible matrix into data
    i <- solve(data)                    # Compute the inverse
    x$setinverse(i)                     # Store the inversed matrix in the container
    i                                   # Return the inversed matrix and finis
}
