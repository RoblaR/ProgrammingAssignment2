## Put comments here that give an overall description of what your
## functions do

## The following function creates a closure and a special spatial matrix to 
##get/set the value of matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setInv <- function(inv) I <<- inv
  getInv <- function() I
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## The following function calculates the inverse of the special "matrix"
##created with the above function. However, it first checks to see if the
##iverse has already been calculated. If so, it `get`s the inverse from the
##cache and skips the computation. Otherwise, it calculates the inverse of
##the data and sets the value of the inverse in the cache via the `setInv`
##function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        I <- x$getInv()
        if(!is.null(I)) {
          message("getting cached data")
          return(I)
        }
        data <- x$get()
        I <- solve(data, ...)
        x$setInv(I)
        I
}

##Test Results:
#> x=rbind(c(2,1),c(3,5))
#> m = makeCacheMatrix(x)
#> m$get()
#[,1] [,2]
#[1,]    2    1
#[2,]    3    5


#> cacheSolve(m)
#[,1]       [,2]
#[1,]  0.7142857 -0.1428571
#[2,] -0.4285714  0.2857143

#> cacheSolve(m)
#getting cached data
#[,1]       [,2]
#[1,]  0.7142857 -0.1428571
#[2,] -0.4285714  0.2857143

