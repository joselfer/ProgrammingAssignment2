## This program creates a special matrix object and store it in cache where the inverse matrix can be calculated and displayed in the console


## This function create a special matrix object and store a matrix and its inverse value in cache for retrieval

makeCacheMatrix <- function(originalMatrix = matrix()) {
        
        #Saves the original Matrix in cache and initialize the value of inverseMatrix to Null
        setMatrix <- function(m) {
                originalMatrix <<- m
                inverseMatrix <<- NULL
        }
        
        #Return the Original Matrix stored in the cache
        getMatrix <- function() originalMatrix
        
        #Store the value of the inverse matrix in inverseMatrix variable
        setInverseMatrix <- function(ivm) inverseMatrix <<- ivm
        
        #Returns the value of the inversed matrix
        getInverseMatrix <- function() inverseMatrix
        
        #Set the functions that the object will have
        list(setMatrix = setMatrix, 
             getMatrix = getMatrix,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
}


## This function accepts a makeCacheMatrix object and calculate inverse matrix if has not being done.
## If inverse matrix has been calculated previously, the function obtains the value from cache insted of recalculating.
## Besides, the function will check if matrix and inverse matrix are reciprocal, if not, the inverse matrix is calculated again,
## This is done because the matrix or inverse matrix can be changed individually

cacheSolve <- function(matrix, ...) {
        inv_matrix <- matrix$getInverseMatrix()
        
        if(!is.null(inv_matrix)) {
                
                #Verify if inverse matrix in cache is actually the inverse of the current matrix. 
                #It calculates the Identity matrix by multiplying the matrix with the inverse matrix in the cache, the result must be a Identity Matrix
                
                master_ident_matrix <- diag(dim(matrix$getMatrix())[1]) #Creates a variable with the Identity Matrix corresponding to the matrix 
                ident_matrix <- matrix$getMatrix() %*% inv_matrix       #Calculate the identitiy matrix resulting of multiplying both matrix
                
                if(!identical(master_ident_matrix, ident_matrix))       #If Mater Identity Matrix and calculated Identity matrix are not equal, 
                {                                                       #it means that matrix in cache has changed whereby the inverse matrix has to be calculated again
                        message("Calculating inverse matrix again as Matrix and Inverse Matrix in cache are not reciprocal...")
                        inv_matrix <- solve(matrix$getMatrix())                
                        matrix$setInverseMatrix(inv_matrix)                                                        
                }
                else message("Getting inverse matrix from cache...")
                
                return(inv_matrix)                                              
        }
        else{        
                message("Calculating inverse matrix...")
                
                inv_matrix <- solve(matrix$getMatrix())                
                
                matrix$setInverseMatrix(inv_matrix)
                
                return(inv_matrix)
        }
}
