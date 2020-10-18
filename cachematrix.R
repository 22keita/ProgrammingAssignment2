## This function creates special matrix that can cache its inverse



makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x << y
                i << NULL
        }
        get <- function() x
        setinv <- function(inverse) i <<- inverse
        getinv <- function() i
        list(set = set, get = get, setinv = setinv, 
             getinv = getinv)
}


## This function returns the inverse of the matrix created above
## It returns cached data if inverse is calculated before,
## otherwise calculate the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
