## Based on example provided in the peer review website
##makeCacheMatrix allows to create a special matrix and four functions with it (set, get,setinverse, getinverse)
##cacheSolve allos to retrieve inverse from Cache if it exists, if not then it computes the inverse and commits
## cache, so that the next time this function is used, it will pull inverse from cache

makeCacheMatrix <- function(x = matrix()) {

#Returns list of functions to create matrix, retrieve matrix, set inverse of matrix and get inverse of matrix
  
  m <- NULL 
  set <- function(y) {
    #send the new matrix to cache and setinverse to null
    x <<- y
    m <<- NULL
  }
  get <- function() x #get the matrix 
  setinverse <- function(inv) m <<- inv #send the inverse of matrix to cache
  getinverse <- function() m # gets the inverse 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## If inverse exists in cache then below function pulls inverse from Cache, else calculate inverse using matrix provided

cacheSolve <- function(x, ...) {
  m <- x$getinverse() 
  if(!is.null(m)) { # check if inverse exists in cache, if so then return that directly
    message("getting cached data")
    return(m)
  }
  data <- x$get() # since inverse do not exist in cache, use data set in cache to calculate inverse
  m <- solve(data)
  x$setinverse(m)
  m
  
}
