## the below two functions generate a square matrix that is cached and then inverted

## this function creates and then caches a matrix. It is assumend that each matrix supplied is invertible

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    
    get<-function()x
    setmean<-function(mean) m<<-mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}


## this function collects the chaed matrix and applies a matrix inversion via the solve function

cacheSolve <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmean(m)
    m
       
}


##an example of implementation
m1<-matrix(c(6,2,8,4), nrow = 2, ncol = 2)
mymatrix<-makeCacheMatrix(m1)
cacheSolve(mymatrix)
