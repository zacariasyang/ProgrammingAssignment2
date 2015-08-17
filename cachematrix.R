## Put comments here that give an overall description of what your
## functions do

## this function is to wrap up a matrix with interface of 4 methods

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                                                  # make new matrix obsoletes the old inversion
    set <- function(y = matrix()) {
        x <<- y                                                  # overwrite with new matrix from outside
        inv <<- NULL                                             # new matrix obsoletes the old inversion from outside
    }
    get <- function() x                                          # get the current matrix
    setinv <- function(inversion) inv <<- inversion              # write in inversion from outside
    getinv <- function() inv                                     # get the current inversion
    list(set = set, get = get, setinv = setinv, getinv = getinv) # return a set of functions
}


## this function is to use aboved-defined methods to interact with matrix
## it first finds if there is inversion previously cached. if not, it calculates one and set it into the matrix wrapper

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()                                        # fetch the inversion through pre-defined interface
        if(!is.null(inv)) {                                      # figure out if there is any inversion cached
            message("getting cached data!!!!!")
            return(inv)
        }
        data <- x$get()                                          # in case of no inversion cached, start to calculate the inversion
        inv <- solve(data, ...)                                  # ... carries additional arguments of solve fucntion
        x$setinv(inv)                                            # write in the inversion
        inv
}
