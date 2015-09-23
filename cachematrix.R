## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function creates a special "matrix", 
## which is a list containing a function to
## set the value of the matrix using $set
## get the value of the matrix using $get
## set the value of the Inverse of the matrix using $setInverse
## get the value of the Inverse of the matrix using $getInverse

## if the matrix has changed values, run makeCacheMatrix again with the new matrix in the argument before running cacheSolve
## or use the set function and enter the new matrix as the argument to update the CacheMatrix

## example output below:
## > source("cachematrix.R")
## > mx<-matrix(1:4,nrow=2,ncol=2)
## > cacheMatrix<-makeCacheMatrix(mx)        # creates the special cache matrix
## > cacheSolve(cacheMatrix)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(cacheMatrix)                # retrieving cached inverse matrix
## getting cached data        
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > 
## > mx<-matrix(2:5,nrow=2,ncol=2)
## > cacheMatrix$set(mx)                      # sets the cache with a new matrix to solve for
## > cacheSolve(cacheMatrix)                  # new inverse calculated
##      [,1] [,2]
## [1,] -2.5    2
## [2,]  1.5   -1
## > cacheSolve(cacheMatrix)                  # retrieve cached inverse matrix
## getting cached data
##      [,1] [,2]
## [1,] -2.5    2
## [2,]  1.5   -1
## > 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                     # creates empty variable which will ultimately store the inverse matrix
        set <- function(y) {
                x <<- y                 # 
                m <<- NULL 
        }
        get <- function() x 
        setInverse <- function(Inverse) m <<- Inverse 
        getInverse <- function() m 
        list(set = set, get = get, 
            setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse() 
        if(!is.null(m)) { 
                message("getting cached data") 
                return(m) 
        }
        
        data <- x$get() 
        m <- solve(data, ...) 
        x$setInverse(m)
        m 
}

