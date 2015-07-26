##      This source is free to do anything you want.
##      By Danilo Pinheiro - Brazil, to Coursera Data Science Specialization
##
##
## The function makeCacheMatrix get a matrix() as unique parameter
## and store in environment. If the parameter x is NULL, then generate
## a empty matrix.
##
## Other functions inside makeCacheMatrix, is a setter to 
## matrix *not inverted* (makeCacheMatrix$set)
## and return the matrix *not inverted* (makeCacheMatrix$get)
## and set the *inverted matrix* (makeCacheMatrix$setSolve)
## and get the *inverted matrix* (makeCacheMatrix$getSolve)
##
## Always setting a matrix not inverted, the matrix inverted is cleaned (NULL)
## 
## This function only store, not solve the matrix.


makeCacheMatrix <- function(x = matrix()) {

        x_inv <- NULL
        set <- function(y) {
                x <<- y
                x_inv <<- NULL
        }
        get <- function() x
        setSolve <- function(y) x_inv <<- y
        getSolve <- function() x_inv
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
        
}


## The function cacheSolve get two parameters: X is function
## to store and ... is other parameters to be passed do solve()
## if necessary. See ?solve.
##
## if the *inverted* matrix are not solved yet (is null)
## then cacheSolve call solve() and x$setSolve() to store.
## otherwise, x$getSolve() return with cached inverted matrix
## 
##

cacheSolve <- function(x, ...) {

        data_inv<-x$getSolve()


        if( !is.null(data_inv)) {
                message("getting cached data")
                return(data_inv)
        }
        message("processing data")
        data_inv <- x$setSolve(solve(x$get(), ...))
        x$setSolve(data_inv)
        data_inv
}
