rm(list = ls())
setwd("C:/Users/Preet/OneDrive - Ursinus College/paid_labor/articleharvestr")

devtools::load_all()
devtools::document()

library(xml2)
library(httr)
library(purrr)

sitemap_url <- "https://www.huffpost.com/sitemaps/sitemap-v1.xml"
extracted_data <- extract_sitemap_links(sitemap_url)
all_links <- extracted_data$links
head(all_links)


article_urls <- fetch_sitemap_articles(sitemap_url,
                                       levels = 1,
                                       start_date = "2023-01-01",
                                       end_date = "2023-12-31")

# View the first few
head(article_urls)