## The function makeCacheMatrix create the matrix with cache

makeCacheMatrix <- function(x = matrix()) {
        ## set the matrix inverse how null, because it's not calculated yet
        inverse <- NULL
        ## function for change the values of matrix necessary case
        set_matrix <- function(y){
                x <<- y
                m <<- NULL
        }
        ## function for acess the matrix values
        get_matrix <- function() x
        ## set the inverse matrix according with input of function
        set_inverse <- function(result) inverse <<- result
        ## acess of matrix inverse
        get_inverse <- function() inverse
        ## list with function defined above
        list(get_matrix=get_matrix, set_matrix=set_matrix,
             get_inverse=get_inverse, set_inverse=set_inverse)
}


## The function cacheSolve calculate the inverse of the matrix
##(if the matrix already have the inverse calculate, just load cache) this occors only for matrix create through for makeCacheMatrix function 

cacheSolve <- function(x, ...) {
        ## verifying if matrix inverse already calculated
        m<-x$get_inverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        ## acess the matrix values
        data <- x$get_matrix()
        ## calculate matrix inverse
        m <- solve(data)
        ## saving matrix inverse on cache
        x$set_inverse(m)
        m
}
