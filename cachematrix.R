## The purpose of this set of functions is to cache the inverse of a matrix rather
## than compute it repeatedly.


## The makeCacheMatrix function creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
        set <- function(y){
               x <<- y
               #Substitutes the vector x with y only in the set funciton.
               m <<- NULL
               #Restores to null the value of m to NULL.
       }
       get <- function() x
       #Returns the vlaue of x.
       setinverse <- function(solve) m <<- solve
       #Sets m.
       getinverse <- function() m
       #Returns the value of m.
       list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
       #Assigns the functions (objects) to elements in a list and returns the list.
	}


## The cacheSolve function computes the inverse of the matrix returned by makeCacheMatrix.
cacheSolve <- function(x, ...) {
	m <- x$getinverse()
        #Verifies the m value stored previously with getmean.
        if(!is.null(m)) {
                message ("getting cached data")
                return(m)
        }
        #If the message is NULL returns the message and m, otherwise continues.
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
       }
