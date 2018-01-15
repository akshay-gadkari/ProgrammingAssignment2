makeCacheMatrix <- function(x = matrix()) {

  a <- NULL
  set <- function(y) {
    x <<- y
    a <<- NULL
  }
    
  get <- function() x
  setmean <- function(mean) a <<- mean
  getmean <- function() a  
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


cacheSolve <- function(x, ...) {
  a <- x$getmean()
  if(!is.null(a)) {
    message("retrieving data")
    return(a)
  }
  
  data <- x$get()
  a <- solve(data, ...)
  x$setmean(a)
  a
}
