## 2 functions that create a matrix and inverse


## Matrix creation
makeCacheMatrix <- function( m = matrix() ) {
	     i <- NULL

    ## Setting the matrix remotely
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    ## Matrix retrieval
    get <- function() {
    	    	m
    }

    ## Matrix inverse
    setInverse <- function(inverse) {
        i <<- inverse
    }

    ## Retrieve inverse
    getInverse <- function() {
        ## Return the inverse property
        i
    }

    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


##  Calc inverse of matrix above if unchanged then function below will retrive cahced version

cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()

    
    if( !is.null(m) ) {
           return(m)
    }

    
    data <- x$get()

    
    m <- solve(data) %*% data

    
    x$setInverse(m)

    
    m
}
