
test_that("Computes the liklihood of a given distribution for data x",{
  x1 <- rgamma(100,3)
  f <- function(theta, x) dgamma(x, shape = theta, log = TRUE)
  func1 <- function(x, interval){
  f <- function(theta, x) dgamma(x, shape = theta, log = TRUE)
  f1 <- function(theta, x)
  {sum(f(theta, x))}
  
  oout<- optimize(f1, maximum = TRUE, interval, x=x)
  return(oout$maximum)
  } 
  interval<-c(0,3)
  result_gamma <- MLEfunc(x1,f,interval)
  expect_identical(result_gamma,func1(x1,c(0,3)))
})