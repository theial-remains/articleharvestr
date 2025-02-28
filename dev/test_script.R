rm(list = ls())
setwd("C:/Users/Preet/OneDrive - Ursinus College/paid_labor/articleharvestr")

devtools::load_all()

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

# step 3: scrape articles and return a dataframe


# Fetch all articles from 2023 for a given news website
article_urls <- fetch_article_urls_by_year("https://www.huffpost.com/sitemaps/sitemap-v1.xml", 2023)

# View the first few URLs
head(article_urls)


