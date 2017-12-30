##R programming assignment 2

makeCacheMatrix <- function(x = matrix()) {
## The function creates a special "matrix" object that can cache its inverse.
## It is a list containing a function to
        #1.  Set the matrix
        #2.  Get the matrix
        #3.  Set the inverse
        #4.  Get the inverse

        m <- NULL
        #Define function to...
        set <- function(y) {
                x <<- y      #Set the matrix (1)
                m <<- NULL   #Clear the cache
        }
        
        #Define function to...
        get <- function() x                                #Get value of matrix (2)
        setInverse <- function(inverse) m <<- inverse      #Set the inverse (3)
        getInverse <- function() m                         #Get the inverse (4)
        
        #Return list with the above functions
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}



cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
## The list above is used as the input to cacheSolve().
        
##The following function calculates the inverse of the special "matrix" created with the above function.        
        m <- x$getInverse()
        #Checks if inverse has been already calculated
        if(!is.null(m)){ 
                #If yes, gets inverse from cache and skips computation
                message("getting cached data")                  
                return(m)
        }
        #Else calculates inverse
        data <- x$get()
        m <- solve(data, ...)
        #Sets the value of the inverse in the cache via the `setInverse`function
        x$setInverse(m)                         
        m
}
