## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
## The return object is a list containing functions to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the matrix inverse
## 4. get the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        inv = x$getinv()

        if (!is.null(inv)){

            message("getting cached data")
            return(inv)
        }

        mat.data = x$get()
        inv = solve(mat.data, ...)

        x$setinv(inv)
        inv
}
