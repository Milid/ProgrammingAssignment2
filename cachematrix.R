## The first function creates a special object - "matrix" that contains a list 
## of functions. These functions are 'setters' and 'getters' for the matrix and 
## its inverse. The second function checks the cache and returnes the inverse 
## from the cache or calculates the inverse and puts it in the cache.


#the function stores and retrieves the object (matrix and its inverse)

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set<-function(y){                                     #sets the matrix and 
        x <<- y                                           #empties cache
        inv <<- NULL
    }
    get <- function() x                                   #returnes the matrix
    
    setinverse <- function(inverse) inv <<- inverse       #sets the inverse 
    
    getinverse <- function() inv                          # returnes the inverse
                                                          # of the matrix 
    
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)

}


## the function returns the cached value of the inverse if it is there or
## calculates the inverse and stores it into the main function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
