## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
		## Initial the inverse is considered as NULL
		inv<-NULL
		## The set function set the input
		set<-function(y){
			x<<-y
			inv<-NULL
		}
		## Use get function to acquire the corresponding value
		get<-function() x
		## Set the inverse 
		setinv<-function(inverse) inv<<-inverse
		## Use getinv function to acquire the corresponding value
		getinv<-function() inv
		list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv<-x$getinv()
		## If already cache an inverse show it
		if(!is.null(inv)){
			message("getting cached data")
			return(inv)
		}
		## If not calculate the reverse and return it
		data<-x$get()
		inv<-solve(data,...)
		x$setinv(inv)
		inv
}
