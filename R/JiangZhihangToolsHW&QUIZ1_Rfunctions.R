#' Calculate Mean, Variane, SD
#'
#' Computes the mean, variance and sd of a vector
#'
#' @param x vector
#'
#' @return list
#' @export
#' @examples
#' func1(rnorm(10))
func1 <- function(x){
  a = sum(x)/length(x)
  b = sum((x-a)^2)/length(x)
  c = sqrt(b)
  return(list(mean=a,var=b,sd=c))
}

#' Calculate Mean, Variane, SD (again)
#'
#' Computes the mean, variance and sd of a vector, but with user checks
#'
#' @param x vector
#'
#' @return list
#' @export
#' @examples
#' func2(rnorm(10))
func2 <- function(x){
  stopifnot(is.numeric(x))
  stopifnot(length(x)!=0)
  stopifnot(is.finite(x))
  stopifnot(!is.na(x))
  stopifnot(!is.nan(x))
  
  a = sum(x)/length(x)
  b = sum((x-a)^2)/length(x)
  c = sqrt(b)
  return(list(mean=a,var=b,sd=c))
}

#' MLE of gamma distribution
#'
#' Computes the liklihood of a gamma distribution
#'
#' @param x vector
#'
#' @return scalar
#' @export
#' @examples
#' func3(rnorm(10))
func3 <- function(x){
  alpha <- pi
  log <- function(alpha)
    sum(dgamma(x, shape = alpha, log = TRUE))
  interval <- mean(x) + c(-1,1) * 3 * sd(x)
  interval <- pmax(mean(x) / 1e3, interval)
  
  oout<- optimize(log, maximum = TRUE, interval)
  return (oout$maximum)
}

#' Weighted mean, var, sd
#'
#' Computes the weighted mean, var, sd
#'
#' @param d data.frame
#'
#' @return list
#' @export
#' @examples
#' data(d)
#' func4(d)
func4 <- function(d){
  
  a = sum(d$x * d$p)
  b = sum(((d$x - a)^2) * d$p)
  c = sqrt(b)
  return(list(mean=a,var=b,sd=c))
  
}

#' Weighted mean, var, sd with user checkes
#'
#' Computes the weighted mean, var, sd with user checks
#'
#' @param d data.frame
#'
#' @return list
#' @export
#' @examples
#' d <- read.table(url("http://www.stat.umn.edu/geyer/3701/data/q1p4.txt"),header = TRUE)
#' func5(d)
func5 <- function(d){
  
  stopifnot(is.numeric(d$x))
  stopifnot(is.numeric(d$p))
  
  stopifnot(length(d$x)!=0)
  stopifnot(length(d$p)!=0)
  
  stopifnot(is.finite(d$x))
  stopifnot(is.finite(d$p))
  
  stopifnot(!is.na(d$x))
  stopifnot(!is.na(d$p))
  
  stopifnot(!is.nan(d$x))
  stopifnot(!is.nan(d$p))
  
  stopifnot(all.equal(sum(d$p),1))
  
  a = sum(d$x * d$p)
  b = sum(((d$x - a)^2) * d$p)
  c = sqrt(b)
  return(list(mean=a,var=b,sd=c))
  
}

#' Highlevel check function
#'
#' Checks and throws error if not numeric, finit, zero lenth, NA, NAN
#'
#' @param x object
#'
#' @return object
#' @export
#' @examples
#' func6(1,Inf)

func6<-function(x,y)
{
  if(!length(y)>0){
    print("length of the second input is not positive")
  }else if(!length(x)>0){
    print("the length of first input is not positive")
  }else if(!is.numeric(x)){
    print("the first input is not numeric")
  }else if(!is.numeric(y)){
    print("the second input is not numeric")
  }else if(sum(is.finite(x))!=length(x)){
    print("the first input has infinite elements")
  }else if(sum(is.finite(y))!=length(y)){
    print("the second input has infinite elements")
  }else if(length(y)!=length(x)){
    print("the lenghs of two input are not equal")
  }else if(!isTRUE(all.equal(sum(y),1))){
    print("the sum of second input is not equal to 1")
  }else if(sum(y>0)!=length(y)){
    print("second input have negative elements")
  }else{
    mu<-sum((x)*y)
    si<-sum(((x-mu)^2)*y)
    data.frame(mean = mu,variance = si, sd = sqrt(si))
  }
}

#' MLE 
#'
#' Computes the liklihood of a given distribution for data x
#'
#' @param x vector
#' @param func function, e.g., `function(theta, x) dgamma(x, shape = theta, log = TRUE)`
#' @param interval vector, i.e., interval for optimize function
#'
#' @return scalar
#' @export
#' @examples
#' x1=rgamma(100,3)
#' func1 = function(theta, x) dgamma(x, shape = theta, log = TRUE)
#' result7_gamma <- func7(x1,func1,c(0,3))
#' result7_gamma
#' 
func7 <- function(x, func, interval){
  
  f7 <- function(theta, x)
  {sum(func(theta, x))}
  
  oout<- optimize(f7, maximum = TRUE, interval, x=x)
  return(oout$maximum)
} 

