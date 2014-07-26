## some functions that cache the inverse of a matrix
## creates a special matrix object that cache its inverse

makeCacheMatrix <- function(x = matrix()) {
i<-NULL
set<-function(y) {
	m<<-y
	i<<-NULL
}

##get the matrix
getmatrix<-function() x
setInverse<-function(inverse) {
	i<<-inverse
}
getInverse<- function() {
	##return the inverse property
	i
}

list(set=set, getmatrix=getmatrix, setInverse=setInverse, getInverse=getInverse)
}


## compute the matrix returned by "makeCacheMatrix". If the ##inverse has already been calculated adn the matrix hasn't ##changed, then the "cachesolve" should retrieve the inverse from ##the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getInverse()
        if(!is.null(m)) {
        	message ("getting cached data")
        	return(m)
        }
        
        ##get the matrix
        data<-x$getmatrix()
        
        ##calculate the inverse using matrix multiplication
        m<-solve(data) 
        x$setInverse(m)
        ## Return the matrix
        m
}
