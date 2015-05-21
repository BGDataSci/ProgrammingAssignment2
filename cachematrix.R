## Put comments here that give an overall description of what your
## functions do

## Makes a function which is a list of functions for retrieving, working on and
##caching results of computations.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL   ##matrix "S"olution inverse initially NULL
        setMatrix <- function(y){
                x <<- y   ##setMatrix Function sets a new Matrix
                s <<- NULL   ## Solution is NULLified because Matrix just man-
                ##ually reset.
                }
        
        getMatrix <- function() x  ##Returns the matrix originally passed as x
        ##unless x has been reset in setMatrix
        
        setSolution <- function(inverse) s <<- inverse   ##Sets the Solution of
        ##the inverse matrix to "inverse" if/ when setSolution() is invoked.
        
        getSolution <- function() s   ##returns the value of the Solution
        ##whether is has been set or solved in the solve function below
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setSolution = setSolution, getSolution = getSolution)  ##puts the
        ##four sub-functions into a list of functions that can be invoked below
        ##using x$getMatrix, x$getSolution, etc.
        
}


## Solve for the Inverse of the Matrix unless the "s"olution already exists and
##has been set,

cacheSolve <- function(x, ...) {
        s <- x$getSolution() ##see if solution has already been set by invoking
        ## getSolution and checking whether it is NULL or not
        if(!is.null(s)) {
                message("getting cached data")
                return(s)   ##returns the value of s previously set or cached
                ##if it is already set.
        }
        matrix <- x$getMatrix()   ##returns the matrix value passed as x to makeCache
        ##Matrix since the value of s was NULL
        s <- solve(matrix, ...)  ##calculate the inverse on the Matrix since it
        ##hasn't already been calculated and/or set.
        x$setSolution(s)
        s   ## Return a matrix that is the inverse of 'x'
}