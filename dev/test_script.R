setwd("C:/Users/Preet/OneDrive - Ursinus College/paid_labor/articleharvestr")
rm(list = ls())

devtools::load_all()

#' title
#'
#' description
#' @param param parameter desc
#' @param param parameter desc
#' @return what it returns
#' @export
ex_function <- function(param, param) {
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

schema <- pull_schema("https://www.example2.com")
print(schema)

