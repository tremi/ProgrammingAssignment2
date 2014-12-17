#Coursera - R Programming Course / Johns Hopkins University - Assignment 2 - December 17th 2014

#Functions for caching inverse of a matrix

#Create cached matrix structure in which inverse matrix can be saved
#Input: invertible numeric square matrix X
#Output: "cache matrix"
makeCacheMatrix <- function(x = numeric()) {  
  
  # *** Error handling for input (even though not required in the assigment as assumed that input is an invertible matrix) ***
  if( class(x) != "matrix") { #Check if input is not a matrix (e.g. list, a single value)
    x <- NULL ;
    message("ERROR: input is not a matrix")
  } else if( mode(x) != "numeric") { #Check if input is not numeric (e.g. matrix with characters)
    x <- NULL ;
    message("ERROR: input is not a numeric matrix")
  } else if ( dim(x)[1] != dim(x)[2] ) { #Check if input is not a square matrix (e.g. 2x5 matrix)
    x <- NULL ;
    message("ERROR: input is not a square matrix")
  }
  #Error handling to be implemented: Check if input is not a invertible square matix (e.g. 2x2 matrix in which all values are 1)
  
  # *** Initial inverse matrix is NULL ***
  m <- NULL 
  
  # *** Cache Matrix internal functions ***
  #Set Cache Matrix
  set <- function(y) {
    x <<- y #Set the natrix
    m <<- NULL #Set the cached inverse matrix as NULL
  }
  #Return the matrix
  get <- function() x
  #Set inverse matrix which is cached
  setInverse <- function(inverse) m <<- inverse
  #Return the cached inverse matrix
  getInverse <- function() m
  
  #List of internal functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

#Get already calculated inverse matrix from memory or calculate inverse of matrix if not already calculated
#Input: "cache matrix" created with "makeCacheMatrix" function
#Output: inverse of the cached matrix
cacheSolve <- function(x, ...) {
  m <- x$getInverse() #Get inverse matrix from memory
  if(!is.null(m)) { #If inverse matrix has been calculated into memory (it is not NULL)
    message("getting cached data") #Tell user that cached inverse is used
    return(m) #Return the inverse matrix
  }
  #If inverse matrix has not been calculated into memory
  data <- x$get() #Get matrix from "cache matrix" structure
  m <- solve(data, ...) #Calculate inverse matrix
  x$setInverse(m) #Put inverse matrix into memory in "cache matrix" structure
  return(m) #Return the inverse matrix
}
