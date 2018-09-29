## Functions are used to create special object that stores matrix and caches its inverse

## This first function creates special matrix which is really a list containing funcion to:
## 1. Set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <-  NULL
  set <- function(y) {
      x <<- y
      i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list( set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    
  )
}


## This function computes the inverse of the special matrix returned by function above.
## if inverse has been already calculated , and matrix has no changed, then this function retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
