## Coursera - R Programming.
## Programming Assignment 2
## Student: Christian Willig.

# The following funtion creates a list containing a function to
# 1. set the value of a matrix
# 2. get the value of a matrix
# 3. set the value of inverse of a matrix
# 4. get the value of inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  # This function follows the same format described in the assignment.
  # Initial state of the matrix, NULL.
  invMatrix <- NULL
  
  # This function stores/saves a Matrix
  setMatrix <- function(inputValue){
      x <<- inputValue
      invMatrix <<- NULL
  }
  
  # This function returns the stored Matrix
  getMatrix <- function(){
    x
  }
  
  # This function sets the value for the cached Inverse Matrix
  setInvMatrix <- function(inputMatrix){
    invMatrix <<- inputMatrix
  }
  
  # This function returns the inverse Matrix
  getInvMatrix <- function(){
      invMatrix
  }
  
  # Encapsulating all functions into a list.
  list(getMatrix = getMatrix, setMatrix = setMatrix, getInvMatrix = getInvMatrix, setInvMatrix = setInvMatrix)

}


#The following function performs the calculation of an inverse matrix created with 
#the above function, makeCacheMatrix(). If the the inverse has been calculated 
#previously then it's returned and no further calculations are performed.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  # This function follows the same format described in the assignment.
  
  # Getting cached value for inverse Matrix in case it has been calculated.
  inverseMatx <- x$getInvMatrix()
  
  # Validating returned value for inverse Matrix.
  if(!is.null(inverseMatx)){
      message("Cached value found!, fetching cached data for inverse Matrix")
      return(inverseMatx)
  }
  
  # In case it didn't find any cached value, let's get the Matrix
  data <- x$getMatrix()
  # ...and calculate the inverse.
  inverseMatx <- solve(data, ...)
  # once calculated, assign new value in cache.
  x$setInvMatrix(inverseMatx)
  
  #Returning inverse Matrix.
  return(inverseMatx)
}
