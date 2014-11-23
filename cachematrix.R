##I made makeCacheMatrix to create a squared matrix and store the inverse value of it
##cacheSolve to used to check if the inverse is already calculated, if not, it will be calculated.

## this function is to create the matrix and store the inverse of it.
makeCacheMatrix <- function(x = matrix()) {   ##I will use matrix() to create a matrix here
    
  inverse <-NULL                            ## The inverse was set to NULL at the beginning
  
    get <- function(){x}                      ## When cacheSolve calling get(), it will give the value of matrix
    
    setinverse <- function(inv = NULL){inverse <<-inv} ##superassignment of inverse by inv from cacheSolve
    
    getinverse <- function(){inverse}  ## When cacheSolve calling getinverse(), it will give the value of inverse
    
    list(get = get,
         setinverse = setinverse,
         getinverse = getinverse) 
}

## this function is to evalute if the inverse is already there for a certain matrix
## if not, then it will use solve to calculate the inverse and store it in the inverse variable above.
 
cacheSolve <- function(x) {
    inverse <- x$getinverse() ## Try to get the value of inverse from makeCacheMatrix()
    if (!is.null(inverse)){   ## This if loop will check if the inverse already had a value,     
           message("getting cached data")   ## If yes, then this message will be printed
           return(inverse)                 ## And the value of inverse will be returned.
    }
    data <- x$get()                        ## If the inverse doesn't have value(NULL), then data will 
                                          ## be set to the value of x(the matrix created before)
    
    inverse <- solve(data)                ## the inverse of the matrix is calculated 
    
    x$setinverse(inverse)                 ## inverse in makeCacheMatrix will be evaluated to INVERSE here.
    inverse                              ## Return a matrix that is the inverse of 'x'
}