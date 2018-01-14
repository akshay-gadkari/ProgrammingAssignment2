makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
    
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m  
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


cacheSolve <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("retrieving data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setmean(m)
  m
}
