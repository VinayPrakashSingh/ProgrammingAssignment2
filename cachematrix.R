#our goal hear is to create two functions with below behaviours:

#Function 1: The should posses setting/getting of matrix which is user 
#input hear and the functions will set the inverse of the user matrix and 
#creating reverse of the same



#Function 1 definition: this function works like a class having four behaviours (i.e.set, get, setInv
# and getInv). it overrides <<- assignment operator so that our results keep intact
#withing our function scope

#we are assuming that the matrix provided is invertible


makeCacheMatrix <- function(x = matrix()) {
  #Declaration 'xinv' named matrix and initializes with NULL
  xinv <- NULL 
  
  # Below it the setter function sets matrix object in makeCacheMatrix function
  # makeCacheMatrix(m1) # passing m1 matrix to the function 
  # makeCacheMatrix$set(m2) # setting m2 as matrix object
  
  set <- function(y) {
    x <<- y
    xinv <<- NULL # it also initialises xinv to null
  }
  
  get <- function() x # return the input matrix
  setInv <- function(inv) xinv <<- solve(x) # set the inversed matrix
  getInv <- function() xinv # return the inversed matrix
  # return a list that contains these functions, so that we can use
  #in cacheMatrix()
  # x <- makeCacheMatrix(testmatrix)
  # x$set(newmatrix) # to change matrix
  # x$get # to get the setted matrix
  # x$setInv # to set the inversed matrix
  # x$getInv # to get the inversed matrix
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
  
  
}


## Calls makeCacheMatrix() to get special matrix created  
## and returns inverse from cache if it exists, otherwise it calculates 
## inverse itself and stores in cache before returning it


cacheSolve <- function(x, ...) {
  ## Check the cache for the inverse
  ##If inverse matrix has been set then return 
  tempMat <- x$getInv()
 
  if(!is.null(tempMat)) {
    message("getting cached data")
    return(tempMat)
  }
  ## Otherwise, retrieve the input matrix and compute its inverse
  data <- x$get()
  tempMat <- solve(data)
  ## Store in the cache
  x$setInv(tempMat)
  ## Return inverse
  tempMat  
}
