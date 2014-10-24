## Created by Marco Conti, SP - Brazil
## Created date 23/10/2014

# This function do:
# 1. Writes the value of "Y" in "X", which will be stored in cache;
# 2. Return the value of variable X (that is in the cache);
# 3. Stores the value of the parameter into the variable 
#    that will be retrieved by the function getInverse, and
# 4. Return the value of variable invMat

makeCacheMatrix <- function(x = matrix()) {
    invMat <- NULL # Initialize local variable (same enviroment of the function) with NULL 
    set <- function(y) {
        x      <<- y     # writes into a cache
        invMat <<- NULL
    }
    get <- function(){ 
        x
    }
    setinverse <- function(inverse) {
        invMat <<- inverse
    }
    getinverse <- function() {
        invMat
    }
    list(set = set
         ,get = get
         ,setinverse = setinverse
         ,getinverse = getinverse)
}


# This function verified If the contents of X are in chache, then returns it the value, 
# but if the value is not into the cache the function calculating solves, 
# put into the cache and return the value.
cacheSolve <- function(x, ...) {
    invMat <- x$getinverse()
    # If the contents of X are in chache, then returns it
    if(!is.null(invMat)) {
        #print("Returning data from cache.")
        message("Returning data from cache.")
        return(invMat)
    }
    # If the contents of X is not in the cache, 
    # then writes its value in the cache and returns this value
    invMat <- solve(x$get())
    x$setinverse(invMat)
    invMat
}
