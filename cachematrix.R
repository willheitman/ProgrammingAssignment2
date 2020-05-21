## These functions provide a means of calculating and then caching the inverse
## of a matrix.  If the inverse has already been calced, the cache can be retrieved

## returns of a list of functions which can set a matrix, retrieve that matrix,
## set a calculated inverse of a matrix, and retrieve that calculated inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## looks to see whether inverse has been calculated, if so returns inverse from cache.
## If not, it calls solve to calc the inverse and sets it to the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}
