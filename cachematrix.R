## Function makeCacheMatrix() 
## Input Parameters  : matrix dataType 
## Output Parameters : special vector 
## Description :   
## It creates a "special" vector that contains a list of the following functions
## (a) setM() -- sets the Matrix 
## (b) getM() -- retrieves the Matrix
## (c) setInverse()  -- Sets the Inverse of the Matrix 
## (d) getInverse()  -- Gets the Inverse of the Matrix
## 

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL   				    ## Initialize 
      set <- function(y) { 
                x <<- y   			    ## sets the matrix
                inverse <<- NULL
      }   
      get <- function() x  			    ## returns the matrix
      setinverse <- function(inv) i <<- inv         ## sets the inverse 
      getinverse <- function() i 		    ## gets the inverse
        
	list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

## Function cacheSolve() 
## Input Parameters  : special vector created using makeCacheMatrix ()
## Output Parameters : inverse of the Matrix
## Description :  The function returns the inverse of the matrix , 
## if the cached copy does not exist, otherwise returns the cached copy 
## and saves precious compute time 

cacheSolve <- function(x, ...) {
        i <- x$getinverse()             ## Gets the inverse
        if(!is.null(i)) {               ## Checks if the inverse exists
                message("getting cached data")   
                return(i)               ## Returns the Cached value
        }

        data <- x$get()       ## Gets the Matrix to calculate the inverse      
        i <- solve(data, ...) ## Inverses the Matrix using the solve() function 
        x$setinverse(i)  ## Uses the Set Function to Cache the Inversed Matrix
        i                ## Returns the Inverse of the Matrix
}
