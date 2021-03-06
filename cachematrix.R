## Matrix inversion using cached memory 

## Carlos Contreras
## carlos.contreras@ualberta.ca

## Creates a special storage for a matrix

makeCacheMatrix  <- function(A = matrix()) {
    Ainv <- NULL
    setMatrix <- function(B) {
        A <<- B
        Ainv <<- NULL
    }
    getMatrix <- function() A
    setInverse <- function(Ainvt) Ainv <<- Ainvt
    getInverse <- function() Ainv
    list(setMatrix = setMatrix, 
         getMatrix = getMatrix, 
         setInverse = setInverse,
         getInverse = getInverse)
}


## Computes the inverse of a matrix in sepcial storage, 
## or retrieves the inverse if it has been already computed

cacheSolve <- function(A, ...) {
    Ainv <- A$getInverse()
    if(!is.null(Ainv)) {
        message("retrieving inverse from cached data")
        return(Ainv)
    }
    Mat <- A$getMatrix()
    Ainv <- solve(Mat, ...)
    A$setInverse(Ainv)
    Ainv
}
