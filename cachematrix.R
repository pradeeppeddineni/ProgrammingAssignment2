## The functions below makeCacheMatrix calculate the inverse of a matrix and saves it
## to the cache such that the next time the user attempts to calculate the
## matrix inverse, the previously saved value is returned instead of
## repeating the calculation.

## # makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

          m <- NULL
          set <- function(y) {
                    x <<- y ## Here we are assigning the input matrix y to the variable x in the parent environment
                    m <<- NULL ## we re-initialize m in the parent environment to null
          }
          get <- function() x ## call the matrix x
          setinverse <- function(inverse) m <<- inverse ## we set the cache m equal to the inverse of the matrix x
          getinverse <- function() m ## return the cached inverse of x
          list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been caclulated. 
## If it is already caluclated it 'get's the inverse from the cache and skips the computation. 
## if inverse is not caluclated it calculates the matrix inverse and sets the value of the inverse 
## in the cache via the 'setinverse' function.

cacheSolve <- function(x, ...) {
          ## Return a matrix that is the inverse of 'x'
          
          m <- x$getinverse()
          if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
          }
          data <- x$get()
          m <- solve(data, ...)
          x$setinverse(m)
          m
}
