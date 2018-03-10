
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
#' f = function(theta, x) dgamma(x, shape = theta, log = TRUE)
#' result_gamma <- MLEfunc(x1,f,c(0,3))
#' result_gamma
MLEfunc<-function(x,f,interval){
  logl<-function(theta){
  sum(f(theta,x))
  }
  oout<-optimize(logl,maximum=TRUE,interval)
  oout$maximum
  }