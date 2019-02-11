## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##This function create a cache Matrix to store the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        ##Set property
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        #Get property
        get <- function() x
        setinv <- function(inverse) i <<- inverse
        getinv <- function() i
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

#This function computes the inverse of a matrix 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #Get inv from x
        inv <- x$getinv()
        #Check if already exists on the cache
        if(!is.null(inv)) {
                message("getting cached result")
                return(inv)
        }
        data <- x$get()
        #Inverse matrix
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
