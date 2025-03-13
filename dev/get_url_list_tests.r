rm(list = ls())

setwd("C:/Users/Preet/OneDrive - Ursinus College/paid_labor/articleharvestr")

devtools::load_all()
devtools::document()

library(xml2)
library(httr2)
library(purrr)
library(stringr)

sitemap_url <- "https://www.cnbc.com/site-map/"

extracted_data <- gu_extract_sitemap_links(sitemap_url)
all_links <- extracted_data$links
all_links

start_date <- "2023-01-01"
end_date <- "2023-01-03"

filtered_links <- gu_filter_links_by_date(all_links, 3, start_date, end_date)
head(filtered_links)

date_pattern <- "/(19[5-9][0-9]|20[0-9][0-9])/?$"

extracted_dates <- str_extract(all_links, date_pattern)

print(raw_year_links)


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


