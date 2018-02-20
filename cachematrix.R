## Two functions that will cache the inverse of a matrix


## create a matrix that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {

  #initialize inverse variable
  
  i <- NULL
  
  #setting the matrix with the following method
  set <- function(matrix) { 
    x <<- matrix
    i <<- NULL
  }
  
  #Get the matrix using the following method
  get <- function(){
    ##return matrix
    x
  }
  
 #set the inverse of mtrix using the following method
  setInv <- function(inverse) {
    i <<- inverse
  }
  
  #get the inverse of matrix using the following method
  getInv <- function(){
    ## return inverse variable
    i
  }
  
  ##return methods list
  list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## compute inverse of matrix returned by "makeCacheMatrix" above
## If inverse has been calculated, then this method should
## retrieve inverse from cache

cacheSolve <- function(x, ...) {
  ## return a matrix that is inverse of x
  m <- x$getInv()
  
  ## if already set, just return the inverse
  if( !is.null(m) ) {
    message("Getting cached data...")
    return(m)
  }
  
  ## get the matrix from our object
  data <- x$get()
  
  ## matrix multiplication gives us the inverse
  m <- solve(data) %*% data
  
  ##set the value of the inverse to our object
  x$setInverse(m)
  
  ## Return the calculated matrix
  m
}
