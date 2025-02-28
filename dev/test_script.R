rm(list = ls())
setwd("C:/Users/Preet/OneDrive - Ursinus College/paid_labor/articleharvestr")

devtools::load_all()

library(xml2)
library(httr)

#' title
#'
#' description
#' @param params1 parameter desc
#' @return what it returns
#' @export
ex_function <- function(params1) {
  # code goes here
}

# step 1: get urls for a year

# step 2 take a random sample of 100 articles per month

# step 3: scrape articles and