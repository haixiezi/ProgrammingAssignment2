## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix is a function which uses matrix as input, returns
## a list which involves
#set the value of the matrix
#get the value of the matrix
#set the inverse of the matrix
#get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(y){
                x <<- y
                inver <<- NULL
        }
        get <- function()x
        setinverse <- function(inverse) inver <<- inverse
        getinverse <- function()inver
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## The following function calculates the inverse of the special
## "matrix" created with the above function. However, it first
## checks to see if the inverse has already been calculated. 
##If so, it gets the inverse from the cache and skips the 
##computation. Otherwise, it calculates the inverse of the data 
##and sets the inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
