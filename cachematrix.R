## Creates a list of four functions to later control if the inverse of the matrix 'i' has already been calculted
## 1-'set' changes the matrix x stored in the main function
## 2-'get' returns the value of the matrix x
## 3-'set' changes the value of the inverse of the matrix x
## 4-'get' returns the value of the inverse of the matrix x
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Control if the inverse of a matrix is already calculated. If yes returns the value, if not calculate it
cacheSolve <- function(x, ...) {
        ## get the inverse of the matrix from the list returned in makeCacheMatrix function
        i <- x$getinverse()
        ## check if inverse of the matrix has already been calculated
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        ## if not check if matrix determintat is equal to zero (to avoid an error message)
        if (det(data)==0) {
                i <- NA
                x$setinverse(i)
        } else {
                i <- solve(data)
                x$setinverse(i)
        }
        ## Return a matrix that is the inverse of 'x'
        i
}


