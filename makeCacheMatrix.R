# makeCacheMatrix is a function that takes a matrix as an 
# argument and returns a list a functions that can be applied to that matrix 
makeCacheMatrix <- function(x = matrix()) {
        
        
        inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, 
             setinv=setinv, getinv=getinv)
}

          
#This functions takes the matrix from makeCacheMatrix  and returns its inverse (calculated or cached)
          
        
cacheSolve <- function(x, ...) {
        
        
        inv = x$getinv()
        
        # Check if cache exist
        if (!is.null(inv)){
                #if it does , return the inverse 
                message("getting cached data")
                return(inv)
        }
        
        # if it doesn't exist calculates the inverse ...
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        #  ... and sets it in the cache via the setinv function.
        x$setinv(inv)
        
        return(inv)
}