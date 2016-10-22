## Put comments here that give an overall description of what your
## functions do. 

## The two functions contained in cacheMatrix.R work together by creating an
## object with memory to recall if it needs to calculate a problem or not. 
## The object functions are in list with the names of the functions 
## and secondly, to determine if the inverse of the matrix has already been calculated 
## (and therefore stored in cache) or whether it's necessary to calculate the inverse and 
## stored in the cache. This way, time is saved by not repeting operations already done. 
## 

## Write a short comment describing this function

## makeCacheMatrix is an object builder. It returns a list of the functions or "habilities"
## that can be called using the $ symbol after the name of the object. It also makes the value
## of x accesible because R retains acces to the environment defined by makeCacheMatrix, 
## including the argument used to start the function.
## The <<- operator assigns values to variables located in parent enviroments of the funtions
## as occurs in set and set inverse funtions.
## The set funcion it is used whenever we don't want to create another object and therefore
## modify the content of the existing object with a new matrix.
## m stores the value of the Solve function once calculated in the second function.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) m<<-inverse
  getinverse<-function() m
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## Write a short comment describing this function

## cacheSolve function reads the value behind x$getinverse and save it in m.Then
## m is evaluated using an if statement to determine whether the inverse of the
## matrix has already been calculated.
## If x$getinverse is not empty, then the if statement is true and the value
## of the inverse of x saved in m is returned and the function ends here because 
## of the return() command.

## If x$getinverse is empty, the if statement becomes false and the execution of
## cacheSolves continues to recall the matrix stored in the cache by calling x$get() 
## calculating its inverse using the solve function. The result is saved in m 
## variable to afterwards be saved in the x object using x$setinverse. Finally, the
## matrix inverse is returned by m because is the last argument of the function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getinverse()
  if(!is.null(m)){
    message("Getting cached data")
    return (m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse (m)
  m
}
