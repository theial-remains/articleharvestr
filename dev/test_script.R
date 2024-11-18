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

# testing find_schema_elements
gs_check_schema("https://www.nytimes.com/sitemap/")

gs_write_schema(
  website_url = "https://www.nytimes.com",
  sitemap_url = "https://www.nytimes.com/sitemap/",
  author_element = ".author",
  title_element = ".title",
  date_element = ".date",
  text_element = ".content",
  year_type = "ol",
  year_class = "css-7ybqih",
  month_type = "ol",
  month_class = "css-5emfqe",
  day_type = "ol",
  day_class = "css-7ybqih",
  article_type = "ul",
  article_class = "css-d7lzgg"
)

schema <- gs_pull_schema("https://www.nytimes.com/sitemap/")
View(schema)

gs_remove_schema("https://www.nytimes.com/sitemap/", every = TRUE)


test_sitemap_url <- "https://www.huffpost.com/sitemaps/sitemap-v1.xml"
test_start_date <- "2015-01-01"
test_end_date <- "2015-01-31"

# Run the function and print results
test_results <- gu_parse_xml_sitemap_date_in_url(test_sitemap_url, test_start_date, test_end_date)
print(test_results)
