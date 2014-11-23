## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 inv_x <- NULL
        set <- function(y) {
                x <<- y
                ## Remove matrix from global environment
                inv_x <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) {
                ## Save matrix to global environment
                inv_x <<- inverse
        }
        getInverse <- function() inv_x
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_x <- x$getInverse()
        if(!is.null(inv_x)) {
                message("getting cached inverse matrix")                
                return(inv_x)
        }
        ## Inverse data matrix and set to global environment
        data <- x$get()        
        inv_x <- solve(data)        
        x$setInverse(inv_x)
        ## Return matrix that is the inverse of 'x'
        return(inv_x)
}
