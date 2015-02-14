## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# cacheSolve computes the inverse of the matrix returned by makeCacheMatrix above. It first checks to see if the
# has already been computed. If so, it gets the inverse from the cache and skips the computation. Otherwise, it 
# computes the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}

# Run
# > x <- rbind(c(1,2), c(3,4))
# > m <- makeCacheMatrix(x)
# > cacheSolve(m)
#     [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5
# > cacheSolve(m)
# getting cached data
#     [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5
# > cacheSolve(m)
# getting cached data
#     [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5

# > y <- rbind(c(1,2), c(4,5))
# > m$set(y)
# > m$get()
#     [,1] [,2]
# [1,]    1    2
# [2,]    4    5
# > cacheSolve(m)
#          [,1]       [,2]
# [1,] -1.666667  0.6666667
# [2,]  1.333333 -0.3333333
# > cacheSolve(m)
# getting cached data
#          [,1]       [,2]
# [1,] -1.666667  0.6666667
# [2,]  1.333333 -0.3333333
