
## This function loads the matrix into cache

makeCacheMatrix <- function(x = matrix()) {
     inverse <- NULL ## This will create object to store the cached inverse
     set <- function(y) { ## This is the function to put the real matrix into our cached matrix object and set its inverse proptery to NULL
          x <<- y
          inverse <<- NULL
     }
     
     get <- function() x ## get function to return the real matrix
     setInverse <- function(inverseSolved) inverse <<- inverseSolved ## set function to store the calculated inverse
     getInverse <- function() inverse ## get function to return the inverse property
     list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) ## list of the methods that are accessible
}


## CacheSolve - This function Inverts the matrix and returns the solution

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     inverse <- x$getInverse()  ## grab cached inverse
     if(!is.null(inverse)) { ## if cached inverse already exists, return it
          message("Getting cached matrix")
          return(inverse)
     }
     data <- x$get() ## if cached inverse doesn't exist, grab the actual matrix, not our version of the matrix
     inverse <- solve(data, ...) ## calculate the inverse with R's solve function
     x$setInverse(inverse) ## store the calcualted inverse in the cachedMatrix object
     inverse ## return the calculation
}
