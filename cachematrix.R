## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# 1. set the matrix
# 2. get the matrix
# 3. set the inverse of the matrix
# 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        
        # set variable i (inverse in this case) to NULL
        i <- NULL
        
        # set function - sets x to the argument y and set i to null
        # <<- operator which can be used to assign a value to an object 
        # in an environment that is different from the current environment
        # "m <<- mean, which means that the variable m of the parent environment"
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        #get returns the value of x (argument of makeCacheMatrix)
        get <- function() {
                x
        }
        
        #sets i in makeCacheMatrix to inv(erse) (argument of makeCacheMatrix)
        setinv <- function(inv) {
                i <<- inv
        }       
        
        # getmean returns the value of i (from makeCacheMatrix)
        getinv <- function() {
                i
        }
        
        #returns a labeled vector of functions set, get, setmean and getmean
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## Write a short comment describing this function
#calculates the inverse of the matrix created with the above function
#However, it first checks to see if the mean has already been calculated
#If so, it gets the mean from the cache and skips the computation.
#Otherwise, it calculates the mean of the data and sets
#the value of the mean in the cache via the setmean function.

cacheinv <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #attempts to get the mean from x (if it was calculated previously)
        i <- x$getinv()
        
        #if not null, a valued was cached, so return i
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        #since it's null, set data to x from makeCacheMatrix
        data <- x$get()
        
        #compute the inverse of data
        i <- solve(data, ...)
        
        #set m in x to computed inverse
        x$setinv(i)
        
        #return mean
        i
}
