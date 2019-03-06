##Homework #1 
##Amarilda Celhaka
##9/4/2018


## Write a function p=proportion(x,a) 
##that returns the proportion of elements
##in a vector x with values less than or equal
##to a given number a.

p <- function(x, a){
     z <- x[x <= a]
     prop = length(z)/ length(x)
  return (prop)
}

##to test the function
p(c(1,1,1,6,6,6),5)



##Write a function that takes a matrix
##as an input and returns the columns of
##the matrix that has the largest and the 
##smallest column sums among all the columns.

columnSums <- function(X){
    ncol <- dim(X)[2]
    nrow <- dim(X)[1]
    s <- 0
    for(i in 1:nrow)
      s <- s + X[i,]
      maxIndex <- which(s ==max(colSums(X)))
      maxCol <- X[,maxIndex]
      minIndex <- which(s == min(colSums(X)))
      minCol <- X[,minIndex]
      
      res <- cbind(maxCol, minCol)
      
      return (res)
}

##to test the function 
X <- matrix(1:18, ncol = 3)
columnSums(X)

      
      
   


##Write your own function that calculates the
##product of two matrices (do not use the R 
##operation %*% in your function).
   
   matrixProd <- function(x, y){
     nrow = dim(x)[1]
     ncol = dim(y)[2]
     res <- matrix(NA, nrow, ncol)
     for(i in seq_along(y[1, ])){
       for(j in seq_along(x[, 1])){
         res[j, i] <- sum(x[j, ] * y[, i])
       }
     }
     res
     
   }
   matrixProd(A,B)
   
   
           
       
   