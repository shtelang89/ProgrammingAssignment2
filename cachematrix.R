## Calculate Matrix inverse and cache results to optimize performance for repeated calls

## Create a "matrix" object and add ability to cache its inverse
## This is a vector of functions to set/retrieve the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    cached_inverse <- NULL
    
    set <- function(y) {
        x <<- y
        cached_inverse <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse) { 
        cached_inverse <<- inverse
    }
    
    getinverse <- function() cached_inverse
    
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


## Calculate inverse of a matrix - retrieve the results from cache if previously calculated

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("Found cached inverse")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}


## Tests

## Sample Matrix
data <- matrix(rnorm(4, mean = 50, sd = 5), 2)
m <- makeCacheMatrix()
m$set(data)

# Check if matrix data populated correctly
if(!is.null(m$get())) {
    print("Data set correctly")
} else {
    print("ERROR: Could not set matrix data")
}

# Check inverse is null
if( is.null(m$getinverse()))
    print("Matrix inverse is not cached")

# Calculate inverse
cacheSolve(m)

# Check inverse is NOT null
if( !is.null(m$getinverse())) {
    print("Matrix inverse is successfully cached")
} else {
    print("ERROR: Matrix inverse is not cached")
}

# second call to calcualte inverse should print the "Found cached inverse" message
cacheSolve(m)