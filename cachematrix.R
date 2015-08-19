## An object generator for an object containing a matrix and its inverse.
## makeCacheMatrix generates the object and its get,set functions for both.

makeCacheMatrix <- function(x = matrix()) {
    matrix_solution <- NULL
    set <- function(some_matrix){
        #sets the new matrix value and resets the solution.
        x <<- some_matrix
        matrix_solution <<- NULL
    }
    
    get <- function() x
    set_solution <- function(inv) matrix_solution <<- inv
    get_solution <- function() matrix_solution
    list(set=set,get=get,
         set_solution=set_solution,
         get_solution=get_solution)
}


## Given a "cacheMatrix" object, it will return the inverse of a matrix.
## If it finds it in the chache, it will simply return the value,
## without calculating the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    result <- x$get_solution()
    if(!is.null(result)){
        message("Found result in cache!")
        return(result)
    }
    else{
        result <- solve(x$get())
        x$set_solution(result)
        result
    }  
}
