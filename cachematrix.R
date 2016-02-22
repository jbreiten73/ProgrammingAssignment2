## This function creates a special matrix which can cache its inverse

makeCacheMatrix <- function (x = matrix() )
{
   i <- NULL
   set <- function(y) {
       x <<- y
       i <<- NULL
 } 
   get <- function() x
   setinverse <- function(inv) i <<- inv  
   getinverse <- function( ) i
   list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
   )
}

## This function returns the inverse of the matrix.
## It first checks if it already has been computed:
## If so,it retrieves the computed inverse, and you will get
## the message "getting cached data".
## If not, it calculates the inverse.

cacheSolve <- function (x,...) 
{
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")

        return(i)
    }
    m <- x$get()
    i <- solve(m,...)
    x$setinverse(i)
    i
}

## Example:
##
## M<-matrix(c(0,1,5,6),nrow=2,ncol=2)
## CM<-makeCacheMatrix(M)
## > cacheSolve(CM)
##      [,1] [,2]
## [1,] -1.2    1
## [2,]  0.2    0
## 
## > cacheSolve(CM)
## getting cached data
##      [,1] [,2]
## [1,] -1.2    1
## [2,]  0.2    0
