#'calculate matrix multiplcation 
#'
#'calculates $x^T A^{-1} x$
#'
#' @param x matrix
#' @param a matrix
#'
#' @return matrix
#' @export
#' @examples
#' load(url("http://www.stat.umn.edu/geyer/3701/data/q2p1.rda"))
#' func8(a,x)
func8<-function(a,x){
  stopifnot(is.matrix(a))
  stopifnot(is.vector(x))
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(a))
  stopifnot(is.finite(x))
  stopifnot(is.finite(a))
  stopifnot(nrow(a)==ncol(a))
  stopifnot(nrow(a)==length(x))
  m=solve(a,x)
  sum(x*m)
}

#'binary operator function
#'
#'calculates $x^T A^{-1} x$
#'
#' @param x matrix
#' @param a matrix
#'
#' @return matrix
#' @export
#' @examples
#' load(url("http://www.stat.umn.edu/geyer/3701/data/q2p1.rda"))
#' a %func9% x
"%func9%" <-function(a,x){
  stopifnot(is.matrix(a))
  stopifnot(is.vector(x))
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(a))
  stopifnot(is.finite(x))
  stopifnot(is.finite(a))
  stopifnot(nrow(a)==ncol(a))
  stopifnot(nrow(a)==length(x))
  m=solve(a,x)
  sum(x*m)
}

#'standardize matrix
#'
#'standardize matrix
#'
#' @param x matrix
#'
#' @return matrix
#' @export
#' @examples
#' load(url("http://www.stat.umn.edu/geyer/3701/data/q2p3.rda"))
#' func10(a)
func10<-function(x){
  stopifnot(nrow(x)>1)
  stopifnot(is.matrix(x))
  stopifnot(is.numeric(x))
  stopifnot(is.finite(x))
  for(i in 1:ncol(x)){
    x[,i]=(x[,i]-mean(x[,i]))/sd(x[,i])
  }
  x
}

#'standardize matrix without loop
#'
#'standardize matrix
#'
#' @param a matrix
#'
#' @return x matrix
#' @export
#' @examples
#' load(url("http://www.stat.umn.edu/geyer/3701/data/q2p3.rda"))
#' func11(a)
func11<-function(x){
  stopifnot(is.matrix(x))
  stopifnot(is.numeric(x))
  stopifnot(is.finite(x))
  FUN<-function(x)
  {
    x<-(x-mean(x))/sd(x)
  }
  apply(x,2,FUN)    
}


#'myapply function
#'
#'it is similar with apply function
#'
#'@param x an array
#'@param MARGIN a vector giving the subscripts which the function will be applied over.
#'@param FUN functions to be applied
#'@param ... optional arguments to FUN
#'@return array
#'@export
#'@examples
#'fred=matrix(1:6,nrow = 3)
#'rownames(fred)=c("red","white","blue")
#'colnames(fred)=c("car","truck")
#'myapply(fred, 1, mean)
myapply <- function(x,MARGIN,FUN,...)
{
  if(length(dim(x))!=2)
  {
    stop("matrix is not 2d")
  }
  if(!(MARGIN %in% c(1,2)))
  {
    stop("margin is not in 1 or 2")
  }
  R=dim(x)[1]
  C=dim(x)[2]
  f= match.fun(FUN)
  if(MARGIN==1)
  {
    result=list()
    for(i in 1:R)
    {
      result[[i]]=f(x[i,],...)
    }
  }else if(MARGIN==2)
  {
    result=list()
    for(j in 1:C)
    {
      result[[j]]=f(x[,j],...)
    }
  }
  return(simplify2array(result))
}








