context("Homework2 and Quiz2 functions")

test_that("func8 calculates $x^T A^{-1} x$",{
  load(url("http://www.stat.umn.edu/geyer/3701/data/q2p1.rda"))
  expect_equal(func8(a,x),as.double(t(x)%*%solve(a)%*%x))
})

test_that("func9 calculates $x^T A^{-1} x$",{
  load(url("http://www.stat.umn.edu/geyer/3701/data/q2p1.rda"))
  expect_equal(a%func9% x,as.double(t(x)%*%solve(a)%*%x))
})

test_that("func10 standardize matrix", {
a=c(1,2,3)
b=c(2,9,8)
c=c(8,1,6)
m=cbind(c(1,2,3),c(2,9,8),c(8,1,6))
expect_identical(func10(m),cbind((a-mean(a))/sd(a),(b-mean(b))/sd(b),(c-mean(c))/sd(c)))
})

test_that("func11 standardize matrix", {
  a=c(1,2,3)
  b=c(2,9,8)
  c=c(8,1,6)
  m=cbind(c(1,2,3),c(2,9,8),c(8,1,6))
  expect_identical(func11(m),cbind((a-mean(a))/sd(a),(b-mean(b))/sd(b),(c-mean(c))/sd(c)))
})