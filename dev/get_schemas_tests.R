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
gs_remove_schema("https://www.huffpost.com/sitemaps/archive/sitemap-index.xml", every = TRUE)

gs_check_schema("https://www.huffpost.com/sitemaps/archive/sitemap-index.xml")

schema <- gs_pull_schema("https://www.huffpost.com/sitemaps/archive/sitemap-index.xml")
View(schema)

# 1: write schema
# Test Call for Adding Huffington Post Schema
gs_write_schema(
  website_structure = "https://www.huffpost.com",
  starting_sitemap = "https://www.huffpost.com/sitemaps/sitemap-v1.xml",
  author_element = "//meta[@name='author']/@content",
  title_element = "//title",
  date_element = "//meta[@property='article:published_time']/@content",
  text_element = "//div[contains(@class, 'content-list-component')]",
  structure = "xml",
  layer1_type = "url_date",
  layer1_class = NA,
  layer2_type = "tag_date",
  layer2_class = "lastmod",
  layer3_type = NA,
  layer3_class = NA,
  layer4_type = NA,
  layer4_class = NA
)

