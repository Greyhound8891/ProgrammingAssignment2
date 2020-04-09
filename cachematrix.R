## These functions allow an inverse matrix to be calculated from a square matrix and cache a previous calculation

## This function makes an object of type list that stores an input matrix
## and its inverse when used in the following cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
        imatrix <- NULL
        set <- function(y) {
                x <<- y ##make new matrix = new input y
                imatrix <<- NULL ##reset imatrix to null
        }
        get <- function() x ##spits out what input matrix is
        setimatrix <- function(new) imatrix <<- new ##takes input of a new imatrix
        getimatrix <- function() imatrix ##spits out new imatrix
        list(set = set, get = get,
             setimatrix = setimatrix,
             getimatrix = getimatrix)
}

## This function takes the object created from makeCacheMatrix (list) and uses 
##it to calculate and store the inverse of the matrix stored in the object, otherwise
##this prints out the cached inverse matrix from a previous calculation


cacheSolve <- function(x, ...) {
        imatrix <- x$getimatrix()
        if(!is.null(imatrix)) {
                message("getting cached data")
                return(imatrix)
        }
        data <- x$get()
        imatrix <- solve(data)
        x$setimatrix(imatrix)
        imatrix
}