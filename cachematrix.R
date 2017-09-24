# makeChacheMatrix & cacheSolve Assignment

# cacheMean can be adapted to find the inverse of a matrix!

# makeChacheMatrix takes a matrix arguement which I set as the example matrix from the fourm
# it then stores global variables m and x for use in cacheSolve

makeCacheMatrix <- function(x = matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)){
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(mean) m <<- mean
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



# cacheSolve takes the arguement from makeCacheMatrix, checks to see
# if an inverse has already been calculated, and if so,
# prints out that it's getting it from cache and then
# prints, otherwise it calculates the arguement inverse
# and outputs that (or if non NxN matrix it prints out error message)

cacheSolve <- function(x, ...){
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  if(nrow(data) == ncol(data)){
  m <- solve(data, ...)
  x$setinv(m)
  m  }
  else print("Matrix not invertable")
}
