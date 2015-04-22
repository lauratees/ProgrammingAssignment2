## Put comments here that give an overall description of what your 
## functions do 
# Package used to calculate inverse of a given matrix, using cache to store results in order to optimize process time.
# Usage: > cacheSolve(makeCacheMatrix(x)) where x is a matrix object

 
## Write a short comment describing this function 
# defines a list of setters & getters used to store matrix and inverse matrix object into the cache
 makeCacheMatrix <- function(x = matrix()) { 
	# reset inverse val
	inv <- NULL
	
	# define function to reset matrix and inverse
	set <- function(y){
	   x <<- y
	   inv <<- NULL
	}
	
	# define function to get matrix
	get <- function() x

	# define function to calculate inverse matrix and set into inv
	setinverse <- function(solve) inv <<- solve
	
	# define function to get inverse matrix
	getinverse <- function() inv
	
	# put four functions into a list
	list(set = set,
		 get = get,
		 setinverse = setinverse,
		 getinverse = getinverse) 
} 
 

## Write a short comment describing this function 
# uses function makeCacheMatrix to check if inverse matrix object already exists in cache, 
# else calculates it and stores in cache  
 
cacheSolve <- function(x, ...) { 
## Return a matrix that is the inverse of 'x' 

  inv <- x$getinverse()
  if (!is.null(inv)){
    message("getting cached data")
	return(inv)
  }
  
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv  
} 
