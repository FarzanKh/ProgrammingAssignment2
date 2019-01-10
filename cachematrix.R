

# As mentioned on the instruction, to create this function, we're supposed to use <<-
#assign a value to an object in an environment that is different from the current environment.
# We define makeCacheMatrix function to first set the value of the matrix, then we define get to get the value of the matrix
# and then set the value of inverse of the matrix and finally get the value of inverse of the matrix
# By doing this assignment I realized Python syntaxes are much easier in compare to R!

makeCacheMatrix <- function(x = matrix()) {
        
            inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    set.inverse.function <- function(inverse) inv <<- inverse
    get.inverse.function <- function() inv
    list(set=set, get=get, set.inverse.function=set.inverse.function, get.inverse.function=get.inverse.function)
}


## The following function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed),
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$get.inverse.function()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$set.inverse.function(inv)
    inv
}
