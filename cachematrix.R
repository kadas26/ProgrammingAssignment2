##Since matrix inversion is a costly computation so caching the inverse of a matrix ##rather than computing it repeatedly is beneficial. Below 2 functions cache the ##inverse of a matrix.
## The first function makeCacheMatrix creates a list containing a function to 
## 1. set the value of the matrix 
## 2. get the value of the matrix 
## 3. set the value of inverse of the matrix 
## 4. get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {

  n <- NULL 
  set <- function(y) { 
  x <<- y 
  n <<- NULL 
   } 
  get <- function() x 
  setinverse <- function(inverse) n <<- inverse 
  getinverse <- function() n 
list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
 } 



## The next function cacheSolve computes the inverse of the special "matrix" returned ##by makeCacheMatrix above.
##It first checks to see if the inverse has already been calculated. If so, it gets ##the inverse from the cache and skips the computation. Otherwise, it calculates the ##inverse of the data and sets the value of the inverse in the cache via the ##setinverse function.

cacheSolve <- function(x, ...) {
     n <- x$getinverse() 
     if(!is.null(n)) { 
         message("getting cached data.") 
         return(n) 
     } 
     data <- x$get() 
     n <- solve(data) 
     x$setinverse(n) 
     n
 } 



##Example : Executing the functions in R

## > x = rbind(c(2, 3), c(2, 2))
## > t = makeCacheMatrix(x)
## > t$get()
##      [,1] [,2]
##[1,]    2    3
##[2,]    2    2

## > cacheSolve(t)

##      [,1] [,2]
## [1,]   -1  1.5
## [2,]    1 -1.0

##Retrieving from cache in the second run

## > cacheSolve(t)
## getting cached data.
##       [,1] [,2]
## [1,]   -1  1.5
## [2,]    1 -1.0
##> 