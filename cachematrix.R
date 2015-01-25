## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
        
        if(class(inversematrix) == "matrix") {
                x$setinverse(inversematrix)
                inversematrix
        } else {
                message("Matrix can't be inversed")
        }
}