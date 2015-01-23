## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function creates a matrix and perform inverse of the matrix.
## A list object is returned as cache.
## In set(), matrix inverse is returned.
## In get(), original matrix is returned if matrix is the same. Otherwise, perform and return inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y) {
                x <<- y
                dimx <<- dim(x)
                m <<- x
                
                solve(x)
        }
        
        get <- function() x
        
        list(set = set, get = get)
}


## cacheSolve returns the matrix by makeCacheMatrix function if existed.
## Otherwise, call set() again to get matrix inverse.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        print(class(x))
        m <- x$get()
        print(class(m))
        
        x <- matrix(unlist(x), ncol = dimx[2])
        print(class(x))
        if(!is.null(m) && is.matrix(x) && is.matrix(m) && dim(x) == dim(m) && all(x == m) ) {           
                message("getting cached matrix")
                return(m)
        }
        
        data <- x$set(x)
        
        data
}
