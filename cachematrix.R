## This file has four functions
## makeCacheMatrix creates an matrix object (not the actual values before set is called)
## cacheSolve is basically an interface to read inverted matrix. Cached value is returned
## if available, otherwise inversion is calculated, stored and returned
## unitTestSystem is a test function that can be used to test the functionality
## usage:
## source("cachematrix.R")
## myMatrix <- unitTestSystem(numMatrices, numToReCreate)
## where numMatrices and numToReCreate are positive integers.
## more comments about what each function does within the function bodies.
## Fourth function is the createMatrix function which just creates one, non-singular
## 3x3 matrix at a time.

## MakeCacheMatrix creates an object for setting and getting a matrix
## as well as it's inversion. By assigning myMatrix <- makeCacheMatrix()
## you can create a matrix object that has two setters and two getters
## for the actual matrix and it's inversion respectively.
## myMat <- NULL
makeCacheMatrix <- function(x = matrix()) {
        ## New matrix object created, set the values for myMatrix and 
        ## it's inversion as NULL
        myMat <- NULL  ##Comment this line and uncomment the line above makeCacheMatrix
        ## function to test lexical scoping
        invMat <- NULL
        ## The actual setter for the matrix object. Has to be called in order
        ## for myMatrix to have a value. From top level call myMatrix$set(matrix)
        set <- function(y) {
                ## Store matrix value. Note the <<- notation
                ## first with lexical scoping we will check the scope where whis function
                ## was defined ie. the makeCacheMatrix function. If myMat is not found,
                ## then we would check the next environment for myMat
                myMat <<- y
                ## Clear inversion as it has not being calculated yet.
                invMat <<- NULL
        }
        ## Matrix getter function. Get the value of the original matrix
        get <- function() myMat
        ## Setter for the inverted matrix. Use by calling myMatrix$setinv(myMatrix$get())
        ## Will throw an error if matrix cannot be inverted.
        setinv <- function(x) {
                if (det(x) == 0) {
                        ##Singular matrix, cannot be inverted.
                        ##Meaningfull error handling needed...
                } else {
                        invMat <<- solve(x)
                }
        }
        ## Getter for the inverted matrix. Used by cacheSolve function
        getinv <- function() invMat
        ## List of setters and getters. These have to be spesified in the list, otherwise
        ## they will not be accessible on the myMatrix object
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve should be called every time we want to retrieve a inverted matrix
## It first checks whether we already have the inversion available by calling 
## myMatrix$getinv() function. If successfull the inverted matrix is returned.
## However if the inversion is not found, then cacheSolve will calculate the 
## inversion, store it in "memory" and return the inversion. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## First let see if the inversion has already been completed
        inv <- x$getinv()
        ## Check whether we succeeded, ie inv is not NULL
        if (!is.null(inv)) {
                message("Inverted matrix found from cache, retreiving...")
                ## Return the chached inverted matrix
                return(inv)
        }
        message("Cache miss. Calculating inversion")
        ## There was a cache miss, we need re-calculate. First get the matrix
        data <- x$get()
        ##Calculate the inversion
        if (det(data) == 0) {
                ## Matrix is singular, inverions cannot be done
                ## meaningfull error handling to be implemented
        } else {
                inv <- solve(data, ...)
        }
        ##Store the calculated inversion in cache
        x$setinv(inv)
        ##Return the inverted matrix
        inv
}


##Unit test function for the code
unitTestSystem <- function(maxNumMatrices, reSample) {
        ##Create testdata
        testdata <- list()
        print("Creating matrix objects for testing")
        for (x in 1:maxNumMatrices) {
                msg <- paste("Iteration:", x)
                print(msg)
                tm <- makeCacheMatrix()
                tm$set(createMatrix())
                tm$setinv(tm$get())
                testdata[[x]] <- tm
        }
        ## Check that cache functionality works
        print("Checking cache functionality")
        for (x in testdata) {
                cacheSolve(x)
        }
        ##return(testdata)
        ## Randomly re-create amout of matrices to be re-sampled
        print("Re-creating part of the matrices")
        changedMatrices <- sample(1:maxNumMatrices, reSample)
        for (x in changedMatrices) {
                msg <- paste("Re-creating matrix", x)
                print(msg)
                testdata[[x]]$set(createMatrix())
        }
        ## Now check that the cache still works
        ## NOTE: you should have a cache miss for the changed matrices
        ## Check that cache functionality works
        print("Checking cache")
        for (x in testdata) {
                cacheSolve(x)
        }
        testdata
}

## Function for creating a new matrix
## Repeat loop used to avoid matrices that ARE singular. Otherwise we will
## get computational erros during creation & testing
createMatrix <- function() {
        repeat {
                tm <- matrix(sample(1:5, 9, replace = TRUE), nrow = 3, ncol = 3)
                if(round(det(tm), 6) != 0) {
                        break;
                }
        }        
        return(tm)
}
