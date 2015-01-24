## Below are 2 functions that create an object that finds stores a matrix and caches it's inverse

makeCacheMatrix <- function(x = numeric()) {
        #reset matInv to NULL
        matInv <- NULL
        #create function to make matrix available to cacheSolve
        setMatrix <- function (y){
                x <<- y
                matInv <<- NULL
        }
        #returns supplied matrix
        getMatrix <- function () x
        #sets inverse of matrix to cache
        setMatInv <- function (InvCalc) matInv <<- InvCalc
        #returns stored inverse of matrix
        getMatInv <- function () matInv
        #create list object of the above functions that can be referenced by cacheSolve
        list (setMatrix = setMatrix, getMatrix = getMatrix, setMatInv = setMatInv, getMatInv = getMatInv)
}


## Checks to see if inverse has been cached and either retrieves it or calculates it as needed

cacheSolve <- function(x, ...) {
        #store data from cache in matInv
        matInv <- x$getMatInv()
        #if there is data (not NULL) cached then it use cached data 
        if(!is.null(matInv)) {
                message("getting cached data")
                #breaks out of function and returns matInv
                return (matInv)
        }
        #if there is no inverse cached do the following:
        #gets supplied matrix from makeCacheMatrix function
        data <- x$getMatrix()
        #calculates inverse of supplied matrix
        matInv <- solve(data, ...)
        #stores inverse of supplied matrix in cache
        x$setMatInv(matInv)
        ## Return a matrix that is the inverse of 'x'
        matInv
}
