#This function creates a special "matrix" object that can cache its inverse.
# It can efficiently help caching the inverse matrix repeatedly.
#We can create a special object that stores a matrix and caches its inverse.
makeCacheMatrix <- function(x = matrix()){
    inv <- NULL
    set <- function(y){
        x <<-y
        inv <<- NULL
    } 
    get <- function() {x}
    setInverse <- function(inverse){inv <<- inverse}
    getInverse <- function(){inv}
    list(set = set, get = get, setInverse = setInverse , getInverse = getInverse)
}
#This is a function computes the inverse matrix created by the function above.
cachesolve <- function(x, ...){
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    mat <- x $ get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}

##For Example
pmatrix <- makeCacheMatrix(matrix(1:4, nrow = 2, ncol= 2))
pmatrix$get()
