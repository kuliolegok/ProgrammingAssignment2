##makeCacheMatrix is a function which stores a list of functions:
##set the value of the matrix (function "set")
##get the value of the matrix (function "get")
##set the value of the inverse matrix (function "setinv")
##get the value of the inverse matrix (function "getinv")

makeCacheMatrix <- function(x = matrix()) {
inv<-NULL
    set<-function(y)
{
    x<<-y
    inv<<-NULL
}
    get <- function() x
    setinv <- function(i) inv <<- i
    getinv<-function() inv
    list(set=set,get=get,setinv=setinv,getinv=getinv)
}


##cacheSolve function is a function which calculates inverse matrix of
## a special "matrix".it first checks to see if the inverse matrix 
##has already been calculated.If so, it gets the value from the cache and 
##skips the computation. Otherwise, it calculates the inverse matrix of the data 
##and sets the value of the mean in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv<-x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
