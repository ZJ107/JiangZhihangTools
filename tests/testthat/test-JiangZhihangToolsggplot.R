
test_that("Wrapper function for ggplot2 for data d",{
  ggplot1=ggplot2::ggplot(d)+ggplot2::aes(x=x, y=p)+ggplot2::geom_point()
  expect_identical(as.character(plotMyData(d)),as.character(ggplot1))
})