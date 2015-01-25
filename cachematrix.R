## Put comments here that give an overall description of what your
## functions do


## function makeCacheMatrix
## creates a matrix with a cachable inverted value (when computed using 'cachesolve')
## params:
##      x - matrix 
##
makeCacheMatrix <- function(x = matrix()) {
        ## the inverse of the matrix is initialized to NULL
        inversematrix <- NULL
        
        set <- function(y) {
                ## if the matrix has changed, it's new value is set 
                ## and the inverse of it is re-initialized
                if(!identical(x,y)) {
                        x <<- y
                        inversematrix <<- NULL
                }
        }
        
        # get the current cached matrix
        get <- function() x
        
        #get and set current inverse of the cached matrix
        setinverse <- function(inverse) inversematrix <<- inverse
        getinverse <- function() inversematrix
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## function cacheSolve
## returns inverted value of CacheMatrix, from cache when it exists, if not it 
## computes it and stores it in CacheMatrix invert value cache.
## params:
##      x - matrix to be inverted
##      ... - further arguments passed to or from other methods
## output:
##      inverted Matrix if x can is invertible
## error:
##      "Matrix can't be inverted" if x cannot be inverted.
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversematrix <- x$getinverse()
        
        ## if the inverse of the matrix is setted, and the solve params have not changed, return the cached data
        if(!is.null(inversematrix))   {
                message("getting cached data")
                return(inversematrix)
        }
        
        data <- x$get()
        
        ## Try to invert matrix, fail silently and inform the user if is it not possible
        inversematrix <- try(solve(data, ...), silent = TRUE)
        
        ## try returns class "try-error" when it fails, and solve returns "matrix", 
        ## if the class of inversematrix is "matrix", the Matrix is invertible and thus
        ## the inverse of the new matrix is stored in cached via setinverse
        if(class(inversematrix) == "matrix") {
                x$setinverse(inversematrix)
                inversematrix
        } else {
                ## message to the user when the matrix is not invertible
                message("Matrix can't be inverted")
        }
}