## cache an inverted matrix, or create new inverted matrix if one is not already cached


## create a cache version of inverted matrix

makeCacheMatrix <- function(matrix1) {
        i <- NULL
        set <- function(y) {
                matrix1 <<- y
                i <<- NULL
        }
        get <- function() matrix1
        setinvert <- function(invert) i <<- invert
        getinvert <- function() i
        list(set = set, get = get,
             setinvert = setinvert,
             getinvert = getinvert)
}


## retrieve inverted matrix from cache (makeCacheMatrix above) or return the inverted matrix for the first time

cacheSolve <- function(matrix1) {
        i <- matrix1$getinvert()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- matrix1$get()
        i <- solve(data)
        matrix1$setinvert(i)
        i
}
