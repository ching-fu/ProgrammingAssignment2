## Put comments here that give an overall description of what your
## functions do

#function makeCacheMatrix can store a matrix and its inverse, 
#and provide 4 methods to access them

#function cacheSolve will take an object returned by makeCacheMatrix
# and return the inverse of the matrix cached by makeCacheMatrix 
 
## Write a short comment describing this function
#input a matrix, store it and return a list include 4 methods

makeCacheMatrix <- function(x = matrix()) {
	m_inv <- NULL;
	#ensure x can have inverse
	if(ncol(x)!=nrow(x)){
		message("please give a square matrix");
		return(NULL);
	}
	set<-function(y){
		x<<-y;
		m_inv <<- NULL;				
	}

	get<-function()x;

	set_inverse<-function(new_inv)m_inv<<-new_inv;
	
	get_inverse<-function()m_inv;

	list(set=set, get=get, set_inverse=set_inverse, 
		get_inverse=get_inverse);

}


## Write a short comment describing this function
#input a list create by makeCacheMatrix and return the inverse of the matrix
#stored. if the inverse exists, just fetch it 

cacheSolve <- function(lx, ...) {
	#input an object lx, ensure it's a list
	if(class(lx)!="list"){
		message("please input a list!!");
		return(NULL);
	}
#	message("the matrix stored is:");
#	print(lx$get());
#	message("the inverse stored is:");
#	print(lx$get_inverse());


	#check if there is an inverse stored
	result<-lx$get_inverse();
	if(is.null(result)){
		#No inverse exist, do and cache it
		message("the inverse doesn't exist. Count the inverse and cache it");
		tmp_inv<-solve(lx$get());
		lx$set_inverse(tmp_inv);
		return(tmp_inv);
	}else{
		#the inverse exist, return it
		message("getting cached data"); 
		return(result);		
	}
	## Return a matrix that is the inverse of 'x'	
}
