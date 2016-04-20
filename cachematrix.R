## There are 2 main functions, one takes a matrix and caches it and outputs a list of other functions. The second then checks to see if it the inverse is cached, returns if it is, calculates, stores and the returns if not.

## Creates a list of functions to be passed on to the cachesolve function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) { ## The set function, it changes what matrix has been stored.
                x <<- y
                m <<- NULL
        }
        get <- function() x  ## The get function, this retrieves the stored matrix.
        setinverse <- function(solve) m <<- solve ##The setinverse function, this sets the inverse of the matrix.
        getinverse <- function() m ##The getinverse function, this retrieves the inverse of the matrix.
        list(set = set, get = get, ## This is the output of the function which is a list of the functions created in this function
             setinverse = setinverse,
             getinverse = getinverse)
}


## It checks to see if the inverse is in the cache, if it is it returns it, if not it calculates it, stores and the then returns it.

cacheSolve <- function(x, ...) {
        m <- x$getinverse() #This tries looks to retrieve the inverse from the cache and assign it to m
        if(!is.null(m)) {  #If m is not null then it returns m and returns the message getting cached data
                message("getting cached data")
                return(m)
        }
        data <- x$get()  # If m is null then it retrieves the matrix from the cache
        m <- solve(data, ...) #Solves it
        x$setinverse(m) #And sets the inverse in the cache
        m #It then returns the inverse
}
