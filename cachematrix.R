# Computing the inverse of a square matrix defining two functions that permit to 
# cache values. This permit to save computational time.

# ## This function permit to create a list of four sub-functions. 
# set = define the matrix
# get = print the matrix
# setinv = store the inverse matrix to cache
# getinv = print the inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) i <<- inverse
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This funtion check the cache for inverse matrix. If the inverse matrix is present, print 
# print the values. If is not available it calculate a new inverse matrix, cache the resul
# and print.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       i<- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i        
        
        
}
