## Put comments here that give an overall description of what your functions do
## The makeCacheMatrix function creates a special matrix, which contains a function to set the value of the matrix, get the value of the matrix, set the value of the inverse, and get the value of the inverse.
## The cacheSolve function calculates the inverse of the special "vector" created with the above function. However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.



## Write a short comment describing this function
## Given below is the makeCacheMatrix function that essentially contains a function to do one of four things : set/get the value of matrix, set/get the value of inverse. 

makeCacheMatrix <- function(x = matrix()) {
 m<- NULL
  set<- function(y) {
    x<<-y
    m<<- NULL
  }
  get<- function() x
  setinverse<- function(inverse) m<-- inverse
  getinverse<- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## This function first checks whether the inverse of the matrix already exits in the cache. If it does, it obtains the value from the cache. Else, it evaluates the inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data<- x$get()
  m<- solve(data, ...)
  x$setinverse(m)
  m
      
}
