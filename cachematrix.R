## This functions compute the inverse of a matrix and store the result in a cache,
## if the inverse of a matrix has been calculated before it results the cache value.  

## Set the value of the matrix, get the value of the matric, set the value of the inverse
## and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        set_inv <- function(inverse) i <<- inverse
        get_inv <- function() i
        list(set = set,
             get = get,
             set_inv = set_inv,
             get_inv = get_inv)
}


## If the inverse has already been calculated it sends the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$get_inv()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$set_inv(i)
        i
}
