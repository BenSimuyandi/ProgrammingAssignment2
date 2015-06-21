## These two functions cache the inverse of a matrix,
## rather than having to compute it repeatedly.
## They assume the input matrix is invertible.

#This function creates a special matrix object to cache its inverse
makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        
        #set sets the value of the matrix stored in the main function
        set <- function(y) {
                x <<- y
                m <<- NULL  #restores the inverse matrix to NUL
        }                   #since old inverse no longer needed
        
        #get returns the matrix provided
        get <- function() x
        
        #setinverse will store the value of the input provided
        #i.e. the input "solve"
        setinverse <- function(solve) m <<- solve
        
        #getinverse will get the stored value
        getinverse <- function() m
        
        #this list stores the 4 functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function competes the inverse of the "matrix" object
## returned by makeCacheMatrix above. If the inverse has already
## been calculated, it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #check if there is already an inverse stored and not NULL
        #This would have been stored by the getinverse function
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        #the code below only runs if there is no stored inverse
        
        #calculate inverse and store as "m"
        data <- x$get()
        m <- solve(data, ...)
        
        #store the new inverse with getinverse function
        #so that it can be retrieved in future if necessary
        x$setinverse(m)
        
        #return the inverse
        m
}
