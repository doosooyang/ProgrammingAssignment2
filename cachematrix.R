## The functions changes a matrix into a 'special' matrix that saves(caches) the inverse of it,
## and use that 'special' matrix to find the inverse without computing it repeatedly.


## makeCacheMatrix returns a 'special' Matrix that includes functions to set, get a matrix 
## and the functions to set, get the inverse of it

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setInv <- function(inverseMatrix) inv <<- inverseMatrix
      getInv <- function() inv
      list(set=set, get=get, setInv = setInv, getInv = getInv)
}


## cacheSolve returns the inverse of a matrix, only computing the inverse if it was not previously computed. 

cacheSolve <- function(x = list(), ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getInv()
      if (!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data)
      x$setInv(inv)
      inv
}
