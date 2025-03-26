rm(list = ls())

setwd("C:/Users/Preet/OneDrive - Ursinus College/paid_labor/articleharvestr")

devtools::load_all()
devtools::document()

library(xml2)
library(httr2)
library(purrr)
library(stringr)


# CNBC
# lv 3
sitemap_url <- "https://www.cnbc.com/site-map/"
extracted_data <- gu_extract_sitemap_links(sitemap_url)
all_links <- extracted_data$links
tail(all_links, 50)

start_date <- "2023-01-01"
end_date <- "2023-01-03"
levels <- 3

filtered_links <- gu_filter_links_by_date(all_links, levels, start_date, end_date)
head(filtered_links)

# lv 2
sitemap_url <- "https://www.cnbc.com/site-map/articles/2023"
extracted_data <- gu_extract_sitemap_links(sitemap_url)
all_links <- extracted_data$links
tail(all_links, 50)

start_date <- "2023-01-01"
end_date <- "2023-01-03"
levels <- 2

filtered_links <- gu_filter_links_by_date(all_links, levels, start_date, end_date)
head(filtered_links)

pattern <- "(january|february|march|april|may|june|july|august|september|october|november|december|\\d{4}-\\d{2})"
matching_links <- all_links[str_detect(all_links, regex(pattern, ignore_case = TRUE))]
matching_links




# huffpost
sitemap_url <- "https://www.huffpost.com/sitemaps/sitemap-v1.xml"

extracted_data <- gu_extract_sitemap_links(sitemap_url)
all_links <- extracted_data$links
head(all_links)

start_date <- "2023-01-01"
end_date <- "2023-01-03"
levels <- 1

filtered_links <- gu_filter_links_by_date(all_links, levels, start_date, end_date)
filtered_links

article_urls <- gu_fetch_sitemap_articles(sitemap_url,
                                          levels = 1,
                                          start_date = "2023-01-01",
                                          end_date = "2023-01-03")
View(article_urls)


