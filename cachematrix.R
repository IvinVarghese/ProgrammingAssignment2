## Put comments here that give an overall description of what your
## functions do

## Two main functions are below. The first does the basic operations of setting and getting values
## for the matrix and its inverse. The second function checks if the inverse is present in the 
## cached memory, if not it calculates the inverse and then sets it in the cached memory.
## the values are being used from a different environment using the <<- operator

## Write a short comment describing this function

## The makeCacheMatrix, creates a special "matrix", which is a list containing
## a function to do the following operations:
## 1. set the value of the matrix.  
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL ##initialize the variable i to be NULL
        set<-function(y){
                x <<- y ## Assigns value to object R from a different environment
                i <<- NULL ## intializes the inverse to be empty
        }
        get <- function() x ## get is a function with parameter x in a different environment
        setinverse <- function(inverse) i <<- inverse ## setinverse sets the value of the variable 
        ##i to value inverse with which this function is called in a different environment
        getinverse <- function() i ## getinverse gets the value of i from the different environment
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        #this is a list of the 4 different operations
}


## Write a short comment describing this function

## the cacheSolve function calculates the inverse of the special "matrix" created with the 
## makeCacheMatrix, but first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. Otherwise it uses 
## the solve function to calculate the inverse of the matrix and sets the value of the inverse
## in the cache via the setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse() ## i is assigned the value of getinverse function in the list x
        if (!is.null(i)){ ## if there is a value of i, this means cache has calculated inverse value
                message("getting cached value of matrix inverse") ## print message
                return (i) ## return the cached inverse value of the matrix. function execution ends
        }
        data <- x$get() ## data is assigned to the get function of the list x
        i <- solve(data) ## solve is used to get the inverse of the matrix stored in data and 
        ##value is assigned to i. so now i becomes a square matrix with dimensions as data
        x$setinverse(i) ## setinverse is used to give the value of i, to store in the cache memory
        i ## value of i is returned
}
