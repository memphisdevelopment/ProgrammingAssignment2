##Date: January/25/2014
##Description: There are two functions included in this file which illustrate
##the behavior of scoping in R. One of them caches the Matrix passed as a parameter
##and the other one solves the inversion of the matrix in question. 
###
##Assumptions: the matrix in question passed as a parameter will always be invertible.
##
##Usage of the functions:
##Step 1. Create an invertible square Matrix (in case you don't have it already): 
##        x <- matrix(rnorm(4), ncol = 2, nrow = 2)
##Step 2. Call the function makeCacheMatrix passing as a parameter your matrix name.
##        specialVector <- makeCacheMatrix(x)
##Step 3. Call the function cacheSolve passing as a parameter the resulting vector from Step 2.
##        cacheSolve(specialVector)
##        In the first execution you will not get anything else other than the inverted matrix. 
##Step 4. Repeat step 3. As you can see there is a message this time saying "getting cached data" 
##        which means the computation was not done rather it was retrieved from the cache. 
##        This step demonstrates that the results are being retrieved from the cache and 
##        we were able to use effectibely the scoping rules in R. 
##Extra Step. Execute the following: solve(x). Compare your results with Step 3 or 4. As you can see
##        the resulting matrix is the same which is the inverted matrix.


##Description of function: The function "makeCacheMatrix" will create a vector from a matrix 
##that will allow upon call to cache the computations for calculating a heavy process such as a
##Matrix Inversion is.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinversion <- function(solve) m <<- solve
  getinversion <- function() m
  list(set = set, get = get,
       setinversion = setinversion,
       getinversion = getinversion)
}


##Description of function: The function cacheSolve will invert a Matrix provided the matrix has not been cached itself.
##If the previous condition is not met, then the inverted matrix will come from the cache and this will allow us to 
##save computer power.
cacheSolve <- function(x, ...) {
  m <- x$getinversion()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinversion(m)  
  m  
}
