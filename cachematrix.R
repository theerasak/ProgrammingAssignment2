## Inverse square metrix with cache
## If inverse metrix is cached , This function will return it from cache but
## it will calculate inverse metrix by "solve" function if the inverse metrix
## is not in cache.

## Example :  if you have metrix "c"
## >  c=rbind(c(1, -1/4), c(-1/4, 1))
## Step 1) Create cache metrix by "makeCacheMatex"
## >  cVector <- makeCacheMetrix(c)
## Step 2) Get inverse metrix by "cacheSolve"
## >  cacheSolve(cVector)
## [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
##
## If you call cacheSolve next times, it will return the inverse metrix from cache.
## > cacheSolve(ct)
## getting cached data
## [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667


## makeCacheMatrix : Create cached metrix vector.

makeCacheMatrix <- function(x = matrix()) {
    invMetrix <- NULL
    set <- function(y) {
        x <<- y
        invMetrix <<- NULL
    }
    get <- function() x
    setInvMetrix <- function(solvedMetrix) invMetrix <<- solvedMetrix
    getInvMetrix <- function() invMetrix
    list(set = set, get = get,
         setInvMetrix = setInvMetrix,
         getInvMetrix = getInvMetrix)
}


## cacheSolve : Calculate inverse metrix if not exist in cache.

cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    invMetrix <- x$getInvMetrix()
    if(!is.null(invMetrix)) {
        message("getting cached data")
        return(invMetrix)
    }
    data <- x$get()
    invMetrix <- solve(data)
    x$setInvMetrix(invMetrix)
    invMetrix
}
