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
  date_element = ".date",
  text_element = ".content",
  xml_structure = "nested",
  year_type = "ol",
  year_class = "year-list",
  month_type = "ul",
  month_class = "month-list",
  day_type = "div",
  day_class = "day-list",
  article_type = "a",
  article_class = "article-link"
)

schema <- gs_pull_schema("https://www.nytimes.com/sitemap/")
View(schema)

gs_remove_schema("https://www.nytimes.com/sitemap/", every = TRUE)



exlinks <- gu_year_links("https://www.nytimes.com/sitemap/", 1990, 1991, tag_class = ?)

exlinks2 <- gu_append_links("https://www.nytimes.com/sitemap/", exlinks)

exlinks3 <- gu_apply_month_links(exlinks2, tag_type = "ol", tag_class = "css-5emfqe")

exlinks4 <- gu_apply_day_links(exlinks3, tag_type = "ol", tag_class = "css-7ybqih")

exlinks5 <- exlinks4[1:2]

exlinks6 <- gu_apply_article_links(exlinks5, tag_type = "ul", tag_class = "css-d7lzgg")

exlinks7 <- exlinks6[1:10]
exlinks7

gu_day_links("https://www.huffpost.com/sitemaps/sitemap-v1.xml")

# TODO create full demo of package so far + explanations
# start by creating 1 example demo (NOT FULL DEMO PAKCAGE)
