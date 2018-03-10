context("Homework1 and Quiz1 functions")

test_that("func1 computes mean, var, sd", {
         x <- 1:10
         var1<-function(x){(1/length(x))*sum((x-mean(x))^2)}
         x_list<-list(mean=mean(x),var=var1(x),sd=sqrt(var1(x)))
         expect_identical(func1(x), x_list)
         })

test_that("func2 computes mean, var, sd", {
  x <- 1:10
  var1<-function(x){(1/length(x))*sum((x-mean(x))^2)}
  x_list<-list(mean=mean(x),var=var1(x),sd=sqrt(var1(x)))
  expect_identical(func2(x), x_list)
  save<-try(func2(NA),silent=TRUE)
  expect_identical(as.character(attr(save,"condition")),"Error: is.numeric(x) is not TRUE\n")
})

test_that("func3 compute MLE of gamma distribution", {
  x1=rgamma(1000,5)
  func1 = function(theta, x) dgamma(x, shape = theta, log = TRUE)
  interval <- mean(x1) + c(-1,1) * 3 * sd(x1)
  interval <- pmax(mean(x1) / 1e3, interval)

  testfunc3<- function(x, func, interval){
    
    f7 <- function(theta)
    {sum(func(theta, x))}
    
    oout<- optimize(f7, maximum = TRUE, interval)
    return(oout$maximum)
  } 
  expect_identical(func3(x1),testfunc3(x1,func1,interval))
})


test_that("func4 computes the weighted mean, var, sd",{
  d <- read.table(url("http://www.stat.umn.edu/geyer/3701/data/q1p4.txt"),header = TRUE)
  m<-weighted.mean(d$x,d$p)
  f<-function(x,y,z){
    return(sum(((x - z)^2) * y))
  }
  expect_equal(func4(d),list(mean=m,var=f(d$x,d$p,m),sd=sqrt(f(d$x,d$p,m))))
})

test_that("func5 computes the weighted mean, var, sd with user checks",{
  d <- read.table(url("http://www.stat.umn.edu/geyer/3701/data/q1p4.txt"),header = TRUE)
  m<-weighted.mean(d$x,d$p)
  f<-function(x,y,z){
    return(sum(((x - z)^2) * y))
  }
  expect_equal(func5(d),list(mean=m,var=f(d$x,d$p,m),sd=sqrt(f(d$x,d$p,m))))
})

test_that("func6 computes the weighted mean, var, sd with user checks",{
 
  expect_identical(as.character(func6(1,Inf)),"the second input has infinite elements")
})

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
  expect_identical(func7(x1,f,interval),func1(x1,c(0,3)))
})







