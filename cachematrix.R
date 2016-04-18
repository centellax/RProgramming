## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not 
##changed), then the cachesolve should retrieve the inverse from the cache.

## makeCacheMatrix: there are 4 points of this function-set the value of the matrix; get the value of the matirx;
##                                                      set the value of the inverse matrix; get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
  ##set the place to store the cached inverse matirx
  m<-NULL
  ##set and get the value of the matrix
  set<-function(y){
        x<<-y
        m<-NULL
  }
  get<-function()x
  ##set and get the value of the inverse matrix
  setinverse<-function(inverse)m<<-inverse
  getinverse<-function()m
  ##return the matrix
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}

##cacheSolve: This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not 
##changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  ##return the inverse if it was cached
  if (!is.null(m)) {
    message("we already have the data cached")
    return(m)
  }
  ##the inverse hasn't been calculated, calculated it right now
  message("we haven't had the data cached")
  data<-x$get()
  m<-solve(data,...)
  x$setinverse(m)
  m
}


