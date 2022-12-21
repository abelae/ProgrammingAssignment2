

## This function creates a chacheable function for inputing matrixes and their inverse

makeCacheMatrix <- function(cachedMat = matrix()) {
  cachedInv <- NULL ## inverse
  setMat <- function(matVal = matrix()) {
    cachedMat <<- matVal 
    cachedInv <<- NULL
  }
  
  getMat <- function() cachedMat
  
  ##setMat inverse variable in parent env to desired value and return the value as a convenience
  setInv <- function(invVal) {
    cachedInv <<- invVal 
    return(cachedInv)
  }
  
  getInv  <- function() cachedInv
  list(setMat=setMat, getMat=getMat, setInv=setInv, getInv=getInv)
}



cacheSolve <- function(cachedMat=usrVal, ...) { ##special matrix provided or create a test 2x2 matrix
  
  calculatedInverse <- cachedMat$getInv() 
  if(!is.null(calculatedInverse)){
    if(is.matrix(calculatedInverse)){
      print("Cached matrix already exists:")
      calculatedInverse
    }
  }
  else{
    matrixToSolve <- cachedMat$getMat()
    cachedMat$setMat
    calculatedInverse <- solve(matrixToSolve)
    print("Your inverse matrix is:")
    cachedMat$setInv(calculatedInverse)
  }


 
}

abelaux = makeCacheMatrix(matrix(data = c(4,3,2,5), nrow=2, ncol=2))
cacheSolve(abelaux)
## Type twice to run
cacheSolve(abelaux)

