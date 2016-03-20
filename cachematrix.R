# makeCacheMatrix creates a "cache matrix" from existing matrix allowing for caching later on
# cacheSolve is just function to find a matrix inverse using solve
#   but that first look if this calculation has already been done and cached


makeCacheMatrix <- function(x = matrix()) {
  #initiate inversed matrix
  i <- NULL
  
  #create a "set" function that initiate x and i when a new cache matrix is created
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  #create a "get" function that just returns initial matrix
  get <- function() x
  
  #create a "setinverse" function that sets the inversed matrix into i
  setinverse <- function(inv) i <<- inv
  
  #create a "getinverse" function that just returns initial matrix
  getinverse <- function() i
  
  #return list of all functions created
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x) {
## Return a matrix that is the inverse of 'x'
  #go get cached inversed 
  i <- x$getinverse()
  
  #if finds a cache inverse, use it
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  #if doesn't find a cached inverse, use get to obtain initial matrix
  data <- x$get()
  
  #compute inverse
  i <- solve(data)
  
  #cache inversed matrix
  x$setinverse(i)
  
  #return inversed matrix
  i
}
