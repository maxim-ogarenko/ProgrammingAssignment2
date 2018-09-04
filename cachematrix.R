makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {  ##takes y as an argument and assigns it to the matrix x
                x <<- y ##creates x, which is then retrieved by get()
                inv <<- NULL ##creates inv, which is then retrieved by get()
                ## "<<-" forwards search into parent environments
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
        ## Returns an inverse version of x
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        dannye <- x$get()
        inv <- solve(dannye, ...)
        x$setInverse(inv)
        inv
}
