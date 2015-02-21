## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        #set the initial inversematrix to NULL
        inversematrix <- NULL
        #catche the input matrix, catche the inversematrix to be NULL
        set <- function(inputmatrix)
        {
                x <<- inputmatrix
                inversematrix <<- NULL
        }
        #get the input matrix
        get <- function() x
        
        #set the inverse matrix
        setinverse <- function(inputinversematrix) inversematrix <<- inputinversematrix
        
        #get the inverse matrix
        getinverse <- function() inversematrix
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse )

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #get the cached inverse matrix
        inversematrix <- x$getinverse()
        if(!is.null(inversematrix))
        {
                message("getting cached inverse matrix")
                return(inversematrix)
        }
        
        #if cached inverse matrix is NULL, meaning not yet cached, then calculate it
        data <- x$get()
        inversematrix <- solve(data, ...)
        
        #set this calcuated inverse matrix to be the cached inverse matrix, and return it
        x$setinverse(inversematrix)
        inversematrix
}
