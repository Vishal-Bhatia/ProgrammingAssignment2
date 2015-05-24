## This function largely follows the examples given in the iunstructions.
## One change that I have made is that instead of same-line constructs, I have used brace brackets to make the code cleaner. 

## The makeCacheMatrix function outputs a special list that actually contains 4 functions:
## 1. set: set the matrix
## 2. get: get the matrix
## 3. setsolve: set the inverse of the matrix
## 4. getsolve: get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() {
    x
  }
  setsolve <- function(solve) {
    m <<- solve
  }
  getsolve <- function() {
    m
  }
  list(set = set, get = get,setsolve = setsolve,getsolve = getsolve)

}


## This function caculates thge mean of the above special list.
## It checks if the inverse has already been calculated, and if yes, outputs the inverse without calculating it.
## If not, it calculates the inverse and also caches it for future reference.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
  message("getting cached data")
  return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

## Regardless, the final output is the inverse of the inputed matrix.