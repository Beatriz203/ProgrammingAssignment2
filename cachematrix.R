
## This function creates a special "matrix" object that can cache its inverse.The object returned is a list of functions.
## To use one of the functions of the list is necessary to subset the main function.
## In this case there are 4 functions within the main function : set, get, setinverse, getinverse.
## The matrix is assumed to be square and invertible.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL                                       ## The variable i holds the inverse of the matrix.
        set <- function(y) { 
                x <<- y                                 ## Changes the matrix stored previously in the main function
                i <<- NULL                              ## Restore the inverse matrix.
        }
        get <- function() x                             ## Returns the matrix x stored in the main function
        setinverse <- function(inverse) i <<- inverse   ## Store the inverse in the variable i
        getinverse <- function() i                      ## Prints the value of i
        list(set = set, get = get,                      ## Puts together 4 functions within 1 main function.
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache, if not the cachesolve should calculate it.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()                           
        if(!is.null(i)) {                               ## Checks if the inverse is already calculated or not (is already stored in get inverse).
                message("getting cached data")          ## If the value of i (the inverse) is not NULL then it retrives the value already in cache.
                return(i)
        }
        data <- x$get()                                 ## If the value of i ( the inverse) is NULL, it calculates the inverse os the matrix, x.
        i <- Solve(data, ...) 
        x$setinverse(i)
        i                                               ## Return a matrix that is the inverse of 'x'
}
