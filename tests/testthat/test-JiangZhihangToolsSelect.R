
  test_that("wrapper for acquiring marginals and other slices of data data2007",{
  fc<-function(x){
  xb<-data.frame(gdpPercap=data2007$gdpPercap,pop=data2007$pop,gdp = data2007$gdpPercap * data2007$pop)
  return(xb)
  }
  m<-fc(data2007)
  n<-selectmydata(data2007)
expect_identical(m,n)
  })