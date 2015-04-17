## This code consists of two functions, makeCacheMatrix, and cacheSolve. Together
## they compute and cache the inverse of a given matrix.  When called, if a 
## value is already cached, this will be returned.  Otherwise the inverse will
## be computed and cached.  MakeCacheMatrix should be called first, followed by
## calling set on its output.  For example, if you set the output to be called
## z, you would call z$set() and feed it a matrix.  You would then call 
## cacheSolve(z) on z.  The first time it computes and caches the inverse, and
## on future iterations it returns the cached value.

## makeCacheMatrix initialises an empty "cache" matrix m and a set of 
## functions you can use to specify the matrix to be cached (set), return this
## input matrix (get), store the inverse of the matrix (setinverse) and return
## this cached inverse (getinverse)

makeCacheMatrix <- function(x = matrix()) {
       m <- NULL
     set <- function(y) {
                x <<- y
                m <<- NULL
        }
     get <- function() x 
     setinverse <- function(solve) m <<- solve
     getinverse <- function() m
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## cacheSolve first checks the "cache matrix" created by makeCacheMatrix to see
## if an inverse is already stored.  If so, it returns this stored value with
## a message indicating that it is cached data.  If not, it calls the get method
## from makeCacheMatrix to obtain the matrix that we need to invert.  It then
## inverts this matrix using the solve() function and stores the result as m.
## Finally it calls the setinverse method of makeCacheMatrix to copy this local
## m to the "cache matrix" m, and returns m.  

cacheSolve <- function(x, ...) {
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