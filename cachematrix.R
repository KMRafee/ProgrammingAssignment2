# `makeCacheMatrix`: This function creates a special "matrix" object
# that can cache its inverse.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)     

}



# Even though the assignment states that the assumption that matrix supplied
# is always invertible - I added one more function for my Learning to check 
# whether the matrix is "Singular" - If singular the process will stop with 
# a message that inverse not possible

isSingular <- function(x) class(try(solve(x),silent=T))=="matrix"



# `cacheSolve`: This function computes the inverse of the special "matrix" 
# returned by `makeCacheMatrix` above. If the inverse has already been 
# calculated (and the matrix has not changed), then `cacheSolve` should 
# retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
       
       m1 <- x$get()
       ## checks whether the matrix is singular. if yes it stops the process
       if (isSingular(m1)==FALSE) {
       
           print("the given matrix is singular, hence inverse not possible")
                
       } else { 
      
        # Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
       
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
       }
}

## Test Run

## Scenario 1 - Non Singular Matrix

## > x = rbind(c(1, -1/4), c(-1/4, 1))
## > m = makeCacheMatrix(x)
## > m$get()
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00

## No cache in the first run
## > cacheSolve(m)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

## Retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached data.
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667


## Scenario 2 - Singular Matrix
## > x = rbind(c(5,3),c(10,6))
## >  m = makeCacheMatrix(c)
## > m$get()

##      [,1] [,2]
## [1,]    5    3
## [2,]   10    6

## > cacheSolve(m)
## [1] "the given matrix is singular, hence inverse not possible"
