## Put comments here that give an overall description of what your
## functions do

## Check if square matrix
isSquareMatrix <- function(x = matrix()) {
    stopifnot(is.matrix(x))
    d = dim(x)
    nr = d[1]
    nc = d[2]
    if (nr == nc) {
        return(TRUE)
    }
    else {
        return(FALSE)
    }
}

## Create a special object that stores a matrix and caches its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # cached inverse
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    if (isSquareMatrix(x)) {
        setInverse <- function(solve) inv <<- solve        
    }
    else {
        stop("NOT a square matrix!")
    }

    getInverse <- function() inv
    
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Compute the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse already in cache (and the matrix has not changed), then
## cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    if (isSquareMatrix(data)) {
        inv <- solve(data, ...)        
    }
    else {
        stop("NOT a square matrix!")
    }
    x$setInverse(inv)
    inv
}

## Tests
A <- matrix(c(1, 2, 3, 4), 2, 2)
objA <- makeCacheMatrix(A)
cacheSolve(objA)  # 1st time computation
cacheSolve(objA)  # retrieved from cache
