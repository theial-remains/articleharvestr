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

# Example call to gu_get_article_links
gu_get_links(
  website_url = "https://www.nytimes.com/sitemap/",
  start_date = "2020-03-11",
  end_date = "2020-03-15",
  id = "nytimes_1728439811_4239"
)

# Define parameters for the month links retrieval
year_link <- "https://www.nytimes.com/sitemap/2003/"
year <- 2003
year_min <- 2003
year_max <- 2004
start_date <- as.Date("2003-03-11")
end_date <- as.Date("2004-06-15")
month_tag_type <- "ol"
month_tag_class <- "css-5emfqe"
website_structure <- "https://www.nytimes.com/sitemap/"

# Call the `gu_apply_month_links` function
month_links <- gu_apply_day_links(
  year_link = year_link,
  year = year,
  year_min = year_min,
  year_max = year_max,
  start_date = start_date,
  end_date = end_date,
  month_tag_type = month_tag_type,
  month_tag_class = month_tag_class,
  website_structure = website_structure
)

# Define parameters for the day links retrieval
month_link <- "https://www.nytimes.com/sitemap/2003/03/"
month <- 3
year <- 2003
year_min <- 2003
year_max <- 2004
start_date <- as.Date("2003-03-11")
end_date <- as.Date("2004-06-15")
day_tag_type <- "ol"
day_tag_class <- "css-7ybqih"
website_structure <- "https://www.nytimes.com/sitemap/"

# Call the `gu_apply_day_links` function
day_links <- gu_apply_day_links(
  month_link = month_link,
  month = month,
  year = year,
  year_min = year_min,
  year_max = year_max,
  start_date = start_date,
  end_date = end_date,
  day_tag_type = day_tag_type,
  day_tag_class = day_tag_class,
  website_structure = website_structure
)
