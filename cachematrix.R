## The first function create a special "matrix" object, which is just a list 
## containing functions for setting the the matrix, getting the matrix, setting the
## matrix inverse and getting the matrix inverse; and the second function returns 
## the inverse of the "matrix" object returned by the above function. If the inverse
## has already been calculated, then the function returns that inverse (provided the
## matrix hasn't changed), and a new inverse isn't calculated.

## This function creates the aformentioned "matrix" object

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function returns the inverse of the aforementioned "matrix" object

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if(!is.null(inv) & data == x$get()){
        print("Getting the Cached Inverse..")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}
