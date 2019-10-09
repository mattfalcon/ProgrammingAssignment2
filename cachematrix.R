## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#makes cache matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  ##begins by setting the inv to NULL as a placeholder for a future value
  set <- function(y){  ##defines a function to set the vector, x, to a new vector, y, and resets the mean, m, to NULL
    x <<- y
    inv <<- NULL  
  }
  get <- function() x  #returns the vector, x
  setInverse <- function(inverse) inv <<- inverse  
  getInverse <- function() inv  #function to get inverse
  #returns the 'special vector' containing all of the functions just defined
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse) 
}

## Write a short comment describing this function
#inverse of matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()  #get cached value inverse
  if(!is.null(inv)){  #if no value return message 
    message("getting cached data")
    return(inv)
  }
  data <- x$get()  #get value of x matrix
  inv <- solve(data) #
  x$setInverse(inv) #
  inv      #return result
}

#test function
test_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
test_matrix$get()
test_matrix$getInverse()
cacheSolve(test_matrix)

##Example: Caching the Mean of a Vector
#Set the value of the vector
#Get the value of the vector
#Set the value of the mean
#Get the value of the mean

#makeVector <- function(x = numeric()) {
#  m <- NULL
#  set <- function(y) {
#    x <<- y
#    m <<- NULL
#  }
#  get <- function() x
#  setmean <- function(mean) m <<- mean
#  getmean <- function() m
#  list(set = set, get = get,
#       setmean = setmean,
#       getmean = getmean)
#}


#cachemean <- function(x, ...) {
#  m <- x$getmean()
#  if(!is.null(m)) {
#    message("getting cached data")
#    return(m)
#  }
#  data <- x$get()
#  m <- mean(data, ...)
#  x$setmean(m)
#  m
#}
