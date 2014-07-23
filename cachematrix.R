## These functions are for 
## calculating inverse of a matrix and cache the result and
## avoid re-computation if result is in cache

## makeCacheMatrix creates a list containing a function to
## set the value of the matrix (set)
## get the value of the matrix (get)
## set the inverse of the matrix (setinv) using solve function
## get the inverse of the matrix (getinv)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve function calculates the inverse of the matrix created with the above function. 
## it first checks if the inverse has already calculated and cached
## If yes, it gets the inverse from the cache. 
## Else, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
