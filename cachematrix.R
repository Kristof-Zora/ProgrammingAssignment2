## These functions give the inverse of a matrix. If it is possible, that is
## if the inverse of the given matrix has been already stored, then give
## the function the inverse matrix from the cache. In the other case,
## computes the inverse of the matrix and stores it.


## makeCacheMatrix gives a list of functions: set, get, setinverse and
## getinverse.
## Calling the function: A <- makeCacheMatrix(matrix(c(2,4,-1,0),2,2))
## It can be gotten the stored matrix A with the get function: A$get().
## We can store its invere matrix by the setinverse function:
## A$setinverse(solve(matrix(c(2,4,-1,0),2,2)))
## After this the stored inverse matrix: A$getinverse()
## The matrix can be setted by the set function:
## A$set(matrix(c(1,1,1,1),2,2))

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve function give back the inverse of the given function x. If the
## inverse matrix has not been sotred, then compute its inverse by the 
## solve function. If the inverse matrix has been stored, then gives the
## invere from cache.
## Calling: after A <- makeCacheMatrix(matrix(c(2,4,-1,0),2,2)), the command
## cachesolve(A) give back the inverse matrix of A (if it is possible, from the
## cache). Firstly computes its inverse and stores. Using again the command
## cachesolve(A) gives the inverse matrix from the cache.Changed the matrix with
## A$set(matrix(c(0,1,0,2,-4,5,-2,0,3),3,3)), computes the inverse matrix of this
## 3 by 3 matrix (because after set, the inverse will be NULL).

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        
        inv <- solve(data, ...)
        
        x$setinverse(inv)
        
        inv
}
