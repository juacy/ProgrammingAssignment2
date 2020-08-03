## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set_matrix <- function(y){
                x <<- y
                m <<- NULL
        }
        get_matrix <- function() x
        set_inverse <- function(result) inverse <<- result
        get_inverse <- function() inverse
        list(get_matrix=get_matrix, set_matrix=set_matrix,
             get_inverse=get_inverse, set_inverse=set_inverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$get_inverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get_matrix()
        o
        m <- solve(data)
        x$set_inverse(m)
        m
}
