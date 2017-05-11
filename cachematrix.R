## Put comments here that give an overall description of what your
## functions do

# this function create object with matrix, set and get methods for it
# also it contains cached value of inverse matrix with set and get methods for it

makeCacheMatrix <- function(x = matrix()) {
    result <- NULL
    set <- function(y) {
        x <<- y
        result <<- NULL #drop cache if matrix change
    }
    get <- function() x
    setinverse <- function(inverse) result <<- inverse
    getinverse <- function() result
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


#firstly, function check cache in x. if there is no cache, then call function solve for matrix
#and cached calculated matix. otherwise get matrix from cache

cacheSolve <- function(x, ...) {
    result <- x$getinverse()
    if (is.null(result)) {
        result <- solve(x$get())
        x$setinverse(result)
    } else {
        message("getting cached data")
    }
    
    result
}
