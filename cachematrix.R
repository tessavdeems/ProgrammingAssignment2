# makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# 1. set the value of the vector
# 2. get the value of the vector
# 3. set the inverse matrix
# 4. get the inverse matrx

makeCacheMatrix <- function(x = matrix()) {
    
    # set the value of the matrix
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # get the value of the matrix
    get <- function() x
    
    # set the inverse matrix
    setinverse <- function(solve) inv <<- solve
    
    # get the inverse matrix
    getinverse <- function() inv
    
    # save the values
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}

# The following function calculates the inverse of the special "matrix" created with the above function. 
# However, it first checks to see if the inverse matrix has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    
    # get value of inverse from cache
    inv <- x$getinverse()
    
    # check if the inverse was already calculated
    if(!is.null(inv)) {
        # give message and return the cached inverse matrix
        message("getting cached data")
        return (inv)
    }
    
    # calculate the inverse of x
    data <- x$get()
    inv <- solve(data, ...)
    
    # set value of inverse into cache
    x$setinverse(inv)
    
    # return inverse value
    inv
}