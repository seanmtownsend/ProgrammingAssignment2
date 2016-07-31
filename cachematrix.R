## Inorder to save time in determining the inverse of a matrix multiple times we can calculate it once and save it to a cache location to be called apon for future use.
## The first funciton makeCacheMatirx will creates funcitons and data, that can be called apon inorder to set and determin the cached data.
## For best practice onece a matix has been created use makecacheMatirx to form the cache then cacheSolve to find the inverse.

## This function creates data that will store information about a matix and its inverse. 
## It will frist set the matirx inverse to null, because, it has not been determend.
## It also creates funcitons that can be called on in other functions to set the vlaues in the data set.

makeCacheMatrix <- function(x = matrix()) {

	matinv <- NULL  									## Initializes the inverse matrix
	set <- function(mat){ 								## Creates the set function
		x <<- mat  									## Sets the matix (in higher lexical scope)
		matinv <<- NULL 								## Sets the matix inverse to null (in higher lexical scope)
	}
	get <- function() x 								## Creates the get function
	setinv <- function(invf) matinv <<- invf 					## Sets the value of the inverse of the matrix (function, in higher lexical scope)
	getinv <- function() matinv 							## Retuns the matrix inverse vlaue (function)
	list( set = set, get = get, setinv = setinv, getinv = getinv) 	## Sets the output list vlaues

}


## This function will determin if the inverse of a matirx has been prevously found. 
## If it has not, it will determin the vlaue of the inverse reurn it to the user and send the result to matrix cache. 

cacheSolve <- function(x, ...) {
	
	matinv <- x$getinv()				    		## Finds the vlaue of matirx inverse
	if(!is.null(matinv)){						## Determins if the vlaue of the inverse is null
		message("Geting cashed inverse matrix.")		## Returns message to indicate that vlaue was previously determined
		return(matinv)						## Returns vlaue previously determined
	}
	else{									## If vlaue of inverse is null calculates the inverse
		mat <- x$get()						## Gets the vlaue of the matrix
		matinv <- solve(mat, ...)				## Finds inverse of matrix
		x$setinv(matinv)						## Sets the vlue of the matrix for future use
		matinv							## Returns vlaue of inverse to user
	}
	
}