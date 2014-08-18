# The two functions here work together to allow cached values for the inversion
# of a Matrix. The Matrix is created with makeCacheMatrix() which returns a List
# of functions to use when manipulating the Matrix. The cacheSolve(matrix) 
# function will return the cached inversion or solve a new inversion if required.

# The makeCacheMatrix() function creates a special matrix which is really a
# list containing functions to:
#     * get the value of the matrix
#     * set the value of the matrix
#     * get the value of the solved matrix
#     * set the value of the solved matrix
# 
# Returns:
#    A List of functions used to manipulate the matrix
#
# Sample Use:
#     x <- makeCacheMatrix()
#     x$set(matrix(c(1,2,11 ,12), ncol=2, nrow=2, byrow=TRUE)) sets the matrix.
#     x$get() returns the matrix data
#     x$getsolve() returns the inverted matrix
#     x$setsolve([inverted matrix]) sets, and caches, the inverted matrix.
makeCacheMatrix <- function(x = matrix()) {
    # Intialize the solved matrix inversion variable
    s <- NULL
    # define the set() function to set the matrix value and re-set the inversion
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    # define the get() function to return the matrix
    get <- function() x
    # define the setsolve() function to set the solved matrix value
    setsolve <- function(slv) s <<- slv
    # define the getsolve() function to get the solved matrix value
    getsolve <- function() s
    # return a List of functions to use when manipulating the matrix
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

# cacheSolve() inverts the matrix created with makeCacheMatrix().
# It will first check to see if the inverted matrix has already been
# solved. If so, it returns the previously inverted matrix. Otherwise,
# it solves the inversion and saves it.
#
# Returns:
#    An inverted matrix.
#
# Sample Use (x is created with the above makeCacheMatrix()):
#    inversion <- cacheSolve(x) will return the cached inversion or solve new.
cacheSolve <- function(x, ...) {
    # Get the previously solved value
    s <- x$getsolve()
    # return the solved value if we have one. Give a message too.
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    # We need to solve the matrix inversion. So get the data..
    data <- x$get() 
    # Solve it..
    s <- solve(data, ...)
    # and set the value
    x$setsolve(s)
    # Return a matrix that is the inverse of 'x'
    s
}
