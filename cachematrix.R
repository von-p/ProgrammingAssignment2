## This pair of functions calculate the inverse of a matrix and cache it when available
## to make a code computationally more efficient
## this code assumes input of a square matrix and which is invertible

## makeCacheMatrix function takes argument of a matrix and create a "special matrix object" which has
## 4 associated functions to retreive and set the matrix as well as its inverse.

makeCacheMatrix <- function(m = matrix()) {
    mat_inv <- NULL
    set <- function(n) {
        m <<- n
        mat_inv <<- NULL
    }
    get <- function() m
    setinv <- function(inv) mat_inv <<- inv
    getinv <- function() mat_inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## cacheSolve takes the argument of the "special matrix object" created above
## checks if matrix inverse has already been computed. If yes, then simply retreives it
## If not, then calculates the inverse and also sets it, so can be retreived in future

cacheSolve <- function(m, ...) {
    inv <- m$getinv()
    if(!is.null(inv)) {
        message("getting cached inverse of the matrix")
        return(inv)
    }
    matrix <- m$get()
    inv <- solve(matrix, ...)
    m$setinv(inv)
    inv
}


