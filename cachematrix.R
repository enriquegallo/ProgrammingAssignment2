## cacheSolve and makeCacheMatrix work together to invert a matrix while storing results for future use
## without requiring the inversion to be re-calculated

## example of using the function:
# Step 1: Define a matrix. This must be a square matrix. You get an error if it isn't.
#       mat1 <- rbind(c(1,2),c(3,4))
# Step 2: Define a makeCacheMatrix function specific to that matrix
#       make1 <- makeCacheMatrix(mat1)
# Step 3: Run cacheSolve to get the inverted matrix
#       cacheSolve(make1)
# You can also run it directly but the results won't be stored:
#       cacheSolve(makeCacheMatrix(rbind(c(1,2),c(3,4))))

## makeCacheMatrix is used by casheSolve to store results

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x<<-y
                m<<-NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## casheSolve either calculates the inversion of a matrix or returns the inverted value if it has already
## been calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #if cacheSolve has already been run for function x (makeCacheMatrix(specific_matrix)) then use the value already calculated (m)
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        #else get the input matrix from function x and calculate new inverted matrix and store as m within function x
        data <- x$get()
        m <- solve(data)
        x$setinv(m)
        m
}