
  #' wrapper for acquiring marginals and other slices of data data2007
  #'
  #'select two speific rows and compute the product
  #'@param x data.frame
  #'
  #' @return list
  #' @export
  #' @examples
  #' data(data2007)
  #' selectmydata(data2007)
selectmydata<-function(x){
  library(magrittr)
  library(tidyverse)
  library(dplyr)
  xa<-data2007%>%
select(gdpPercap, pop) %>% 
mutate(gdp = pop * gdpPercap)
  return(xa)
  }