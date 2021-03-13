## These are two functions that are used to create a special object that stores 
## a numeric matrix and caches its inverse

## This function accepts an matrix argument x, and calculates its inverse
## It then stores the matrix in a cache m.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL               #clear values of m from prior executions
        set <- function(y) {    # define set() function to set the matrix
                x <<- y         # Set x 
                m <<- NULL      # clear cache
        }
        get <- function() x     # retrieve x from parent envrmt makeCacheMatrix()
        setinverse <- function(inverse) m <<- inverse # Set the inverse
        getinverse <- function() m      # get the inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)   
}


## This function checks to see if the inverse exists, 
## If not, then it calculates the inverse of the matrix created

cacheSolve <- function(x, ...) {
        m <- x$getinverse()     # checks to see if the inverse exists
        if(!is.null(m)) {       # by checking if m contains a value vs null
                message("getting cached data")
                return(m)       # Returns a cached inverse of matrix 'x'
        }
        data <- x$get()         # get the matrix
        m <- solve(data, ...)   # solve() creates the inverse of a matrix
        x$setinverse(m)         # cache the inverse matrix in m
        m                ## Returns a calculated inverse of matrix 'x'
}

