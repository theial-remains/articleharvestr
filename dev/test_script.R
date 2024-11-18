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
gs_check_schema("https://www.huffpost.com/sitemaps/archive/sitemap-index.xml")

# Test Call for Adding Huffington Post Schema
gs_write_schema(
  website_url = "https://www.huffpost.com",
  sitemap_url = "https://www.huffpost.com/sitemaps/archive/sitemap-index.xml",
  author_element = "//meta[@name='author']/@content",
  title_element = "//title",
  date_element = "//meta[@property='article:published_time']/@content",
  text_element = "//div[contains(@class, 'content-list-component')]",
  structure = "xml",
  layer1_type = "year",
  layer1_class = "sitemap-year",
  layer2_type = "month",
  layer2_class = "sitemap-month",
  layer3_type = "day",
  layer3_class = "sitemap-day",
  layer4_type = "article",
  layer4_class = "sitemap-article"
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
