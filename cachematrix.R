# cachematrix.R
# A suite of functions to cache time-consuming matrix inversion computations
#
# Defines a pseudo "matrix" class which manages the cache,
# and a cacheSolve() function to compute the inverse.
#
# Example:
# m1 <- makeCacheMatrix(matrix(rnorm(16), 4, 4))
# cacheSolve(m1)
#
# Based on https://github.com/rdpeng/ProgrammingAssignment2/blob/master/cachematrix.R


# Returns a special "matrix" object (list) that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    cachedInverse <- NULL

    # Accessors to the underlying matrix data
    get <- function () x
    set <- function (newMatrix) {
        x             <<- newMatrix
        cachedInverse <<- NULL
    }

    # Convenient way to call cacheSolve()
    getInverse <- function () cacheSolve(obj)

    getCachedInverse <- function () cachedInverse
    setCachedInverse <- function (inverse) cachedInverse <<- inverse
    obj <- list(get = get,
         set = set,
         getInverse = getInverse,
         getCachedInverse = getCachedInverse,
         setCachedInverse = setCachedInverse)
}


# This function computes the inverse of the special "matrix"
# returned by `makeCacheMatrix`.
#
# If the inverse has already been calculated (and the matrix has not changed),
# then the cached result will be returned.
#
# Returns a matrix that is the inverse of 'x'
cacheSolve <- function (x, ...) {
    inverse <- x$getCachedInverse()

    if (is.null(inverse)) {
        inverse <- solve(x$get())
        x$setCachedInverse(inverse)
    }

    inverse
}
