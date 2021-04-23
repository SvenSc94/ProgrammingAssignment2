## Put comments here that give an overall description of what your
## functions do

## This Function represent NULL-Objects with N, then sets the value of
## the matrice with the set-function. The "<<-" is used to be able
## to set values from other labels/ environments. In the next step,
## I get the Value of the vector, then set the Value of the inverse
## and get the Value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
              x <<- y
              i <<- NULL
        }
        get <- function() {x}
        setmean <- function(inverse) {i <<- inverse}
        getmean <- function() {i}
        list(set = set, get = get,setmean = setmean,getmean = getmean)
}


## This Function computes the inverse of the matrix returned by the
## makeChacheMatrix-Function. If the Inverse has already been
## calculated, the chachesolve retrieves the inverse form the cache.
## Firstly, we create the function and tell it to get the inverse
## from an Object x and create the Object i with it. Next step: If the
## inverse has allready been calculated, a message gets printed and i
## gets returned. If its not allready been calculated,
## the solve-function and the setInverse-Function are used.Then, i
## is printed to the Console.

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(inv)) {
                message("allready been calculated in cache")
                return(i)
        }
        mat <- x$get()
        i <- solve(mat, ...)
        x$setInverse(i)
        i
}
