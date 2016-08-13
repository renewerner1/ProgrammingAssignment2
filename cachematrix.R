## This function creates a cached "matrix" like object that can cache its inverted equivalent

makeCacheMatrix <- function(x = matrix()) {

  ## creates an object that contains four functions: 
    ##1) setinv() 
    ##2) getinv() 
    ##3) set() 
    ##4) get() 
    ##and includes two data objects being x and inv
    
                                                           ## x initialized as a function arg
        inv <- NULL                                        ## inv is getting initialized
            set <- function(y) {
                x <<- y                                    ## assigns y to x in the parent environment
                inv <<- NULL                               ## assigns NULL to inv in the parent environment
            }
        get <- function() x                                ## retrieval of x from the parent environment
        setinv <- function(solve) inv <<- solve            ## assigns input argument to the value of inv in the parent environment
        getinv <- function() inv                           ## retrieval of inv from the parent environment
        list (set = set, get = get,                        ## set names to use extract operator, return list to parent environment
              setinv = setinv,
              getinv = getinv)
}

## This function computes the inverse of the "special" matrix returned by myCacheMatrix.
## if the inverse has already been computed (and the matrix has not changed) then cacheSolve takes the inverted matrix from the cache

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinv()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
