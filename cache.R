# The first function  creates a special "matrix" vector
#that
#    set the value to Matrix
#    get the value of Matrix
#    set the value of the Inverse of Matrix
#    get the value of the Inverse of Matrix

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inver <<- inverse
  getInverse <- function() inver
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


#This function firstly check if the matrix inverse has already been calculated. If so, output from cache.
#Otherwise, it calculates the inverse of the matrix.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inver <- x$getInverse()
   ##Return inverse if it's in the cashe
  if (!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  ##Get the matrix
  matri <- x$get()
  inver <- solve(matri, ...)
  x$setInverse(inver)
  ##output
  inver
}
