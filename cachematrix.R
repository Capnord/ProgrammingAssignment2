## General description = application of lexical scoping, creates a pair of functions that cache the inverse of a matrix if already calculated


makeCacheMatrix <- function(x = matrix()) {
        
        ## initially assigned NULL
        inv <- NULL
        
        ## a setter which can set/assign a new inversable matrice to the argument x
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## a getter which retrieves the value of the argument x 
        get <- function() x
        
      
        ## a setter which assigns the value of z to inv in the parental environment
        set_inv <- function(z) inv <<- z
        
        ## a getter which retrives the value of inv
        get_inv <- function() inv
        
        ## returnes a liste in form of makeCacheMatrix type object
        list(set = set, get = get,
             set_inv = set_inv,
             get_inv = get_inv)

}


## cacheSolve returns a cached value of the inversed x, or calculate the inversed value of x if x is a new object.

cacheSolve <- function(x, ...) {
        
        ## retrieves the value of the original arguments
        inv <- x$get_inv()
        
        ## if inv is not a NULL, meaning inv has been arealdy populated from cachesolve
        if(!is.null(inv)) {
                message("getting cached inversed matrix")
                return(inv)
        }
        
        ## otherwise, when inv is NULL, meaning the arg is a new makeCacheMatrix type object
        else {matrix <- x$get()
        inv <- solve(matrix)
        
        ## using set_inv() to asssign the new value to the object "inv" 
        x$set_inv(inv)
        
        ## return the invesed matrice
        inv}
        
}
