## makeCacheMatrix function is to create a special "vector", which is a list containing a function to 
# 1. set value of matrix, 
# 2. get value of matrix, 
# 3. set inverse of matrix, 
# 4. get inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  # c is a variable to store cache inverse matrix
  invM <-NULL
  # sets value of the matrix
  set=function(y){
    y<<-x
    c<<-NULL
  }
  # function for cacheSolve to extract value of matrix
  get=function(){
    x
  }
  # function to store cache inverse matrix 
  setinverse=function(inverse){
    # regardless of values of invM in the external environment, set invm to be what is entered as the argument
    invM<<-inverse
  }
  # function to call the inverse matrix
  getinverse=function(){
    invM
  }
  # last line of the makeCacheMatrix function to return the set/get functions and their names
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  #to check if the inverse has already been calculated
  invM<-x$getinverse()
  if(!is.null(invM)){
    message("getting cache data")
    return(invM)
  }
  # if invM is null, get the matrix from the cache and inverse it
  else{
    # to get the matrix
    data=x$get()
    # to calculate the inverse matrix
    invM=solve(data)
    # to store the inverse matrix in the cache
    x$setinverse(invM)
    invM
  }
}

x=matrix(1:4,2,2)
y=makeCacheMatrix(x)
cacheSolve(y)
