makeCacheMatrix <- function(x = matrix()) {
        ## x: invertible matrix
     
        inv = NULL
        set = function(y) {
                

		# <<- assign a value to an object in an environment different from the current environment. 

                x <<- y
                inv <<- NULL

        }
        get = function() x
        setinv = function(solve) inv <<- solve 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x = matrix()) {
        ## x: output of makeCacheMatrix()
        ## return: inverse of the original matrix input to makeCacheMatrix()
        

	## it use the inverse that has already been calculated instead of calculate again the value
        

	inv = x$getinv()
        
        

        if (!is.null(inv)){
               ## get the value from the cache when the has already been calculated.. 
                message("getting cached data")
                return(inv)
        }
  
        ## If is hasn´t been calculated then calculates the inverse 

        data = x$get()
        inv = solve(data, ...)
        
        ## sets the value of the inverse in the cache.

        x$setinv(inv)
        
        return(inv)
}
