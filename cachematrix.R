makeCacheMatrix <- function(x = matrix()) {
    m <- NULL # resets the matrix inverse to null
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x # gets whatever x was given in the set() function
                        # NB: x there implicitly returns whatever x was set to
    setmatrix <- function(matrix) m <<- matrix
    getmatrix <- function() m
    
    # makeCacheMatrix returns a list of four functions
    # x$set(y), defined above, is a function that caches the argument y
    #           which should be an invertible square matrix
    # x$get() prints the invertible square matrix y
    # x$setmatrix() sets y-inverse
    # x$getmatrix() gets whatever the environment has been told y-inverse is
    #               regardless of whether it's actually correct
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


cacheSolve <- function(x, ...) {
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
        # NB: Function ends here if the matrix inverse has already been set, 
        #     regardless of whether it's actually correct
    }
    data <- x$get()
    m <- solve(data) # solves for the matrix inverse and sets it into the object, x
                     # aka the argument cacheSolve was passed
    x$setmatrix(m)
    m
}

# NB: To test this code, I tried
# > mat <- makeCacheMatrix(A) #where A is an invertible matrix
# > cacheSolve(mat)
# > mat$getmatrix() #which returns the computed matrix inverse
# > mat$getmatrix() %*% mat$get() #which should return an identity matrix

# acknowledgements to https://github.com/DanieleP/PA2-clarifying_instructions
# which explained functions that store lists of other functions
# and https://class.coursera.org/rprog-032/forum/thread?thread_id=576 which
# offers a script to test the user-made makeCacheMatrix() and solveCache() functions
