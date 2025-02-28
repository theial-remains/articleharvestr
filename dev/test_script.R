rm(list = ls())

setwd("C:/Users/Preet/OneDrive - Ursinus College/paid_labor/articleharvestr")

devtools::load_all()
devtools::document()

#' title
#'
#' description
#' @param params1 parameter desc
#' @return what it returns
#' @export
ex_function <- function(params1) {
  # code goes here
}

# scrape_articles done
# get_urls done

# TODO fix verbose

# step 1: get urls for a year
sitemap_url <- "https://www.huffpost.com/sitemaps/sitemap-v1.xml"
article_urls <- gu_fetch_sitemap_articles(sitemap_url,
                                          levels = 1,
                                          start_date = "2023-01-01",
                                          end_date = "2023-01-31")

# step 2 take a random sample of 100 articles per month
article_urls <- sa_sample_article_urls(sitemap_url,
                                        year = 2023,
                                        month_start = 1,
                                        month_end = 2)
head(article_urls)

# step 3: scrape articles and return a dataframe
