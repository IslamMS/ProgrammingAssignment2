## The following functions work together to create a square invertible matrix
## and make the inverse of the matrix available in the cache environment
##

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##1.  set the value of the matrix

##2.  get the value of the matrix

##3.  set the value of the inverse

##4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

    #Store the initial value to NULL
    inverse_matrix <- NULL
 
    #Creates the matrix
    set <- function(y) {
        x <<- y
        inverse_matrix  <<- NULL
    }

    # Get the value of the matrix
    get <- function() x

    # Invert the matrix and store it in inverse_matrix
    setinverse<- function(inverse) inverse_matrix  <<-inverse

    #Get the inverted matrix from inverse_matrix
    getinverse <- function() inverse_matrix 

    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

    # Get the matrix stored in inverse_matrix
    inverse_matrix  <- x$getinverse()

    # if the inverse of the matrix has already been calculated
	
    if (!is.null(inverse_matrix)) {
        message("getting cached inverse matrix")
        return(inverse_matrix )
    } 


    # otherwise, it calculates the inverse of the matrix
    else {
	inverse_matrix <- solve(x$get())
        x$setinverse(inverse_matrix)

    #Display the inverted matrix
    return(inverse_matrix)
    }
}
