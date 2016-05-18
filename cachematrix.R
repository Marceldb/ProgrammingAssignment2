## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## create the matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
     
        inv <- NULL # where the inversion will be stored
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function
## Calculate the inverted matrix 

cacheSolve <- function(x, ...) {
        
        inv = x$getinv()
        
        # if the inverted matrix already exists take it from the cache
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        # if the inverted matrix does not exist, calculate it 
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        x$setinv(inv)
        
        return(inv)
}
