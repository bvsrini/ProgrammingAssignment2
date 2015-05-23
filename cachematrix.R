################################################################################
# Function makeCacheMatrix stores a list of functions.This makes it 
# possible for the calling function to calculate the inverse of matrix and 
# cache it. So if the inverse of the matrix is sought again, it can be 
# returned rather than computing it. There are 4 functions stored and 
# returned as a list to the caller to access.
# The functions stored are :
#
#  set     -  make it possible to cache the matrix y passed to this function.
#             Variable x is assigned the value of y through the deep assignment 
#             operator( <<- ) to make this possible.Here the invm variable 
#             holds the inverse and is set to NULL since the value of the matrix 
#             is changed 
#  getMat   - retuns the matrix that is stored within this environment
#  getInvMat- retuns the inverse of the matrix that is stored within this 
#             environment in the invm variable. 
#  setIvMat - sets the inverse of a matrix within this environment.variable invm 
#             holds the inverse of the matrix x through a deep assignment 
#             ( <<- )operator.The caller computes the inverse and sets the 
#             inverse through this function.it is for the caller to ensure that 
#             the matrix and inverse stored are in sync. (i.e invm is actually 
#             the inverse matrix of matrix x)
#
# function - makecaheMatrix
# Args:
#   x: matrix for which the inverse needs to be calculated.
#  
# Returns:
#   a list of all functions stored  (set, getMat,setInvMat,getInvMat) in this 
#   environment
#
# function - set
# Args:
#   y : matrix for which the inverse needs to be calculated.
#  
# Returns:
#   no  explict return . However  this will return NULL since this is the 
#   last assignment done
#
# function - getMat
# Args:
#   None  
# Returns:
#   returns the  matrix that is stored in x
#
# function - getInvMat
# Args:
#   None  
# Returns:
#   returns the inverse of the matrix that is stored in invm
#
# function - setInvMat
# Args:
#   (a computed inverse) matrix
# Returns:
#   returns the same matrix that is stored in invm
################################################################################

makeCacheMatrix <- function(x = matrix()) {
        invm <-NULL
        set <- function (y){
                x<<-y
                invm<<-NULL
        }
        
        getMat<- function() {
                x
        }
        getInvMat <- function() {
                invm
        }
        setInvMat <- function(z) {
                invm <<-z
        }
        
        list (set = set, 
              getMat = getMat, 
              getInvMat = getInvMat, 
              setInvMat = setInvMat)
}

################################################################################
# Function cacheSolve computes the inverse of a matrix.Since it is assumed that 
# the inverse of a matrix is a costly operation , it uses makecacheMatrix to 
# store the original matrix passed and the inverse computed.  The function first 
# determines if the matrix passed is invertible else throws a message.
# if the matrix is invertible then the function computes the inverse of the 
# matrix if not already cached.By using the matrix property,that if  matrix 
# multipled by it's inverse should give an identity matrix, the function 
# determines if the inverse is already cached. 
#
# Ref:http://stackoverflow.com/questions/11995832/inverse-of-matrix-in-r
#
# Args:
#    x    a list of functions returned by makeMatrixCache
#    ... argument is used for passing extra arguments to  solve function that 
#        calculate inverse of a matrix
# Returns:
#   (inverse of) a cached matrix or nothing if inverse cannot be calulated
################################################################################
cacheSolve <- function(x, ...) { 
        
        ## get the matrix cached from makeCahceMetrix
        mtx <- x$getMat()           
        
        ## if the matrix is not a squre matrix or is a singular matrix 
        ## solve give an error. exit gracefully
        
       if (nrow(mtx) != ncol(mtx)) {
             message("Matrix is not square,cannot compute inverse")
                return 
        } else if (trunc(det(mtx)) == 0) {
                        message("Matrix is singular,cannot compute inverse")
                        return 
                
        }
        else { 
                inv <- x$getInvMat()  ## get the inverse matrix already computed
                I<- diag(nrow(mtx))   ## create a Identity matrix
                
                ## If inverse retured is not NULL then check to see if multiplying 
                ## the matrix and the inverse creates an identity matrix.
                ## if a identity matrix is returned then the inverse was already
                ## calculated then return the cached inverse of the matrix. 
                ## The round function is used else the matrix returned is very 
                ## very small and will not be comparable to I. 
                
                if ( !is.null(inv)  ) {
                        if (identical(round(mtx %*% inv), I)) {   
                                
                                message("getting cached data for matrix")                       
                                return(inv)                                            
                        }
                } else {
                        ## Recompute the inverse of the matrix since this s a 
                        ## new matrix,cache the computed inverse of the 
                        ## matrix and return the inverse of the matrix
                        
                        m <- solve(mtx, ...)                                  
                        x$setInvMat(m)                                       
                        m                                                    
                } 
                
        }
        
}

