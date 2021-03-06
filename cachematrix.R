## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse
## This function retruns list of four wraped functions
## 1. set matrix
## 2. get matrix
## 3. set/calc inverse of the matrix
## 4. get inversed matrix

makeCacheMatrix <- function(x = matrix()) {
    ## initializing inverse matrix to NULL
    inv_x <- NULL
    ## set function to cache matrix and inverse matrix (initialized to NULL)
    set <- function(y) {
        x <<- y
        ##inv_x <<- NULL
        inv_x <<- NULL
    }
    ## get function to retrive matrix from cache
    get <- function() x
    ## setinverse function to compute inverse matrix & cache it
    ## uses solve function of R to compute inverse matrix
    setinverse <- function(solve) inv_x <<- solve
    ## getinverse function to retrive inverse matrix from cache
    getinverse <- function() inv_x
    ## returns list containing four functions
    list(set = set, get = get, setinverse = setinverse, 
         getinverse = getinverse)
}


## This function
## 1. retruns the inverse matrix from cache if already been calculated
## 2. else computes inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Retrieves inverse matrix using makeCacheMatrix list functions
    inv_x <- x$getinverse()
    ## checking for already computed inverse
    if(!is.null(inv_x)) {
        ## if inverse matrix variable contains NULL value
        message("getting cached data")
        ## return the cached inverse matrix
        return(inv_x)
    }
    
    ## if inverse matrix in not computed then
    ## get the cached matrix via makeCacheMatrix get function
    data <- x$get()
    ## compute the inverse matrix using solve function in R
    inv_x <- solve(data, ...)
    ## set the inverse matrix in cache via makeCacheMatrix setinverse function
    x$setinverse(inv_x)
    inv_x
}
