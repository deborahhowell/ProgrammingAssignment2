## Efficient Matrix Inversion
## 20160303  
##
## DESCRIPTION: 
## This is a set of two functions that do a matrix inversion and cache the
## result.  When the matrix inversion is called, it first checks if it has 
## been inverted, if so, the cached solution is retrieved, if not the matrix is
## inverted and the result returned and cached.  Since matrix inversion is 
## expensive to compute, these functions make it more efficient by not re-
# calculating an inversion that's already been done.


## ==========================================================================
## makeCacheMatrix
## This function creates the set and get functions to store the matrix inversion
## solution.  It returns a list containing these set and get functions
## ==========================================================================

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL  ## Initialize the inversion solution to NULL
    
    ## Create a function called set 
    set <- function(y) {
        x <<- y  ## Set the passed-in value (a matrix) to local variable x
        inv <<- NULL  ## initialize the inverse solution to NULL
    }
    
    ## Create a function called get that returns the matrix
    get <- function() x
    
    ## Create a fuction called setInv that sets the variable inv
    setInv <- function(soln) inv <<- soln
    
    ## Create a function that returns the inverse solution
    getInv <- function() inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
    
}
## ==========================================================================
## ==========================================================================


## ==========================================================================
## cacheSolve
## This function performs the matrix inversion and returns a matrix that is the
## inverse
##
## Arguments:
    ## x - list created by makeCacheMatrix()
    ## ... - other aruguments to pass to "solve" function
## ==========================================================================

cacheSolve <- function(x, ...) {
    ## Retrieve the cached inverse solution, if any
    inv <- x$getInv()
    
    ## If it has already been computed and is unchanged, retrieve and return the 
    ## cached data
    if(!is.null(inv) && ) {
        message("getting cached data")
        return(inv)
    }
    
    ## Get the matrix to be inverted
    data <- x$get()  
    
    ## Invert the matrix
    inv <- solve(data, ...)
    
    ## Cache the inversion solution
    x$setInv(inv)
    
    ## Return the inversion solution
    inv
}
## ==========================================================================
## ==========================================================================
