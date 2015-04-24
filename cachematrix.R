
## makeCacheMatrix function creates a special "matrix" object 
# that can cache its inverse and cacheSolve function takes makeCacheMatrix function's 
# output as input and computes the inverse of the special "matrix" returned by 
# makeCacheMatrix. If the inverse has already been calculated (and the matrix has not changed), 
# then the cacheSolve should retrieve the inverse from the cache.



## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {m <- NULL
                set <- function(y) {
                         x <<- y
                         m <<- NULL
                }
                get <- function() x
                setinv <- function(solve) m <<- solve
                getinv <- function() m
                list(set = set, get = get,
                setinv = setinv,
                getinv = getinv)                                           
}


## cacheSolve function computes the inverse of the special matrix returned by
# makeCacheMatrix and if the inverse of the matrix has already been calculated,
# the function will retrieve it from the cache.

cacheSolve <- function(x, ...) { 
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m   ## Returns a matrix that is the inverse of 'x'
        
}

# Example:

# Let d be any arbitrary square matrix like:

d=matrix(1:4,2,2)

a <- makeCacheMatrix(d)

cacheSolve(a)   ## Returns a matrix that is the inverse of 'd'

# Whwn we run cacheSolve(a) for the same matrix for the second time,
# first it shows the message "getting cached data"
# and then it gives the same result as the first run:

cacheSolve(a)   ## First shows the message "getting cached data" and then returns a matrix that is the inverse of 'd'