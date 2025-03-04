rm(list = ls())

devtools::load_all()
devtools::document()

library(xml2)
library(httr)
library(purrr)
library(stringr)

sitemap_url <- "https://www.huffpost.com/sitemaps/sitemap-v1.xml"
extracted_data <- gu_extract_sitemap_links(sitemap_url)
all_links <- extracted_data$links
head(all_links)

start_date <- "2023-01-01"
end_date <- "2023-12-31"
levels <- 1

filtered_links <- gu_filter_links_by_date(all_links, levels, start_date, end_date)
head(filtered_links)

extracted_data <- gu_extract_sitemap_links("https://www.huffpost.com/sitemaps/archive/sitemap-2023-01-01-v1.xml")
all_links <- extracted_data$links
head(all_links)


sitemap_url <- "https://www.huffpost.com/sitemaps/sitemap-v1.xml"
article_urls <- gu_fetch_sitemap_articles(sitemap_url,
                                          levels = 1,
                                          start_date = "2023-01-01",
                                          end_date = "2023-01-03")

head(article_urls)


