setwd("C:/Users/Preet/OneDrive - Ursinus College/paid_labor/articleharvestr")
rm(list = ls())

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


# testing find_schema_elements
gs_check_schema("https://www.nytimes.com/sitemap/")

gs_write_schema(
  website_url = "https://www.nytimes.com",
  author_element = ".author",
  title_element = ".title",
  date_element = ".pubdate",
  text_element = ".content",
  xml_structure = "nested_ymdl"
)

schema <- gs_pull_schema("https://www.nytimes.com/sitemap/")
print(schema)

exlinks <- gu_year_links("https://www.nytimes.com/sitemap/", 1990, 2000)

exlinks2 <- gu_append_links("https://www.nytimes.com/sitemap/", exlinks)

exlinks3 <- gu_month_links("https://www.nytimes.com/sitemap/2000/", ol_class = "css-5emfqe")


