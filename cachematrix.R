

## Thanks to these two functions, it is possible to use special kinds of 
## matrices that go with
## their inverse counterpart, which gets calculated only once. This can be
## a real time saver.


## The function makeCacheMatrix creates a list, that we will call a "special 
## matrix". This list contains the matrix x (the input of the function), and 
## the inverse of the matrix x. Since this function only "creates" the special 
## matrix and does not do any calculation, the inverse Matrix is first set to 
## NULL. To get the matrix itself or its inverse , the user must refer to the
## elements $get() or $getInverse().
## To set the matrix itself (done upon creation) or to set the inverse Matrix
## once it is calculated, the user must refer to the elements $set(y) or
## $setInverse(inv), with y the matrix, and inv the inverse Matrix.

makeCacheMatrix <- function(x = matrix()) {
      inverseMat <- NULL
      set <- function(y){
            x <<- y
            inverseMat <<- NULL
      }
      get <- function() x
      setInverse <- function(inv) inverseMat <<- inv
      getInverse <- function() inverseMat
      list(set= set, get = get, setInverse = setInverse,
           getInverse = getInverse)
      
}


## This function named cacheSolve calculates the inverse of the "special matrix"
## x. If the inversion had already been done once, it just sends the cached 
## inverse matrix. If not, it calculates it and puts it to the cache of the
## list referred to in the input, the so-called "special matrix".

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getInverse()
      
      #First we look at if the matrix has already been inversed once:
      if(!is.null(m)) {
            print("getting cached data")
            return(m)
      }
      
      # If the execution gets to here, it means that no inverse matrix had been
      # calculated:
      #Then we get the data from the input
      data <- x$get()
      #We inverse the matrix
      m <- solve(data)
      #We set it to the cache
      x$setInverse(m)
      cacheSolve <- m
}

