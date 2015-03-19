## Sample run:
## > x = rbind(c(1, -1/2), c(-1/2, 1))
## > m = makeCacheMatrix(x)
## > m$get()
## [,1] [,2]
## [1,]  1.0 -0.5
## [2,] -0.5  1.0

## > cacheSolve(m)
## [,1] [,2]
## [1,] 1.3333333 0.6666667
## [2,] 0.6666667 1.3333333


## Retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached data
## [1,] 1.3333333 0.6666667
## [2,] 0.6666667 1.3333333
## > 


# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse

makeCacheMatrix <- function(matrix1 = matrix()) {
        # inv will store the cached inverse matrix
        inv <- NULL
        
        # Setter for the matrix
        set <- function(y) {
                matrix1 <<- y
                inv <<- NULL
        }
        
        # Get variable for the matrix
        get <- function() matrix1
        
        # Set variable for the inverse
        setinv <- function(inverse) inv <<- inverse
        
        # Get variable for the inverse
        getinv <- function() inv
        
        # Return the matrix with our newly defined functions
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# cacheSolve: Compute the inverse of the matrix. If the inverse is already
# calculated before, it returns the cached inverse.
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        
        # If the inverse is already calculated, return it
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # The inverse is not yet calculated, so we calculate it
        data <- x$get()
        inv <- solve(data, ...)

        # Cache the inverse
        x$setinv(inv)
        
        # Return it
        inv
        
}