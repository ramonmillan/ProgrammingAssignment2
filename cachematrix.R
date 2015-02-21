##Created 21-02-2015 by Ramon Millan http://www.ramonmillan.com
##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


## makeCacheMatrix creates a special matrix object
makeCacheMatrix <- function(x = matrix()) 
{

inverse_x <-NULL ##sets de value of inverse_x to NULL

set <- function(y)
	{
  	x <<- y ##caches de matrix so that cacheSolve can check whether it has changed
  	inverse_x <<-NULL
	}

get <- function() x

setinverse<-function(inverse) 
	{ 
	inverse_x<<- inverse 
	}

getinverse <-function() inverse_x 

list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## cacheSolve calculates de inverse of the matrix created with makeCacheMatrix
cacheSolve <- function(x) 
{

inverse_x <- x$getinverse()
    
if(!is.null(inverse_x)) ##checks if the inverse cache is available and returns it
	{
      	message("getting cached inverse matrix")
      	return(inverse_x)
	}

else ##if the inverse cache is not available, computes, caches and returns it
	{
	startinginverse_x <- x$get()
	inverse_x <- solve(startinginverse_x) #the function solve calculates de inverse of the matrix
	x$setinverse(inverse_x)
	return(inverse_x)
	}
}
