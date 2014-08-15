## The below two functions cache the value of inverse of any matrix.
## This saves time and makes computation faster as calculating the inverse of a matrix
## is a very costly computation.
## The advantage is, if a matrix is not changing and we need its inverse again and
## again in loop we can just compute its inverse once and use the cached value.

## This function takes a matrix as input and returns a "special matrix".
## This "special matrix" is actually a list containing :
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function takes the "special matrix" as input and calculates the inverse.
## However it checks if the inverse has already been calculated. If so, it gets the
## inverse from the cache and skips the computation. Otherwise, it calculates the 
## inverse of the data and sets the value of the inverse in the cache via setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
