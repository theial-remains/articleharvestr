rm(list = ls())

setwd("C:/Users/Preet/OneDrive - Ursinus College/paid_labor/articleharvestr")

devtools::load_all()
devtools::document()

# scrape all articles
sitemap_url <- "https://www.huffpost.com/sitemaps/sitemap-v1.xml"

article_urls <- gu_fetch_sitemap_articles(sitemap_url,
                                          levels = 1,
                                          start_date = "2024-01-01",
                                          end_date = "2025-03-25")
View(article_urls)

article_urls2 <- gu_remove_duplicates(article_urls)

# store all articles
ss_store_articles(
  article_data = article_urls,
  news_site = "huffpost",
  overwrite = FALSE
)

test_data <- ss_pull_articles(
  start_date = "2024-01-01",
  end_date = "2025-03-25",
  url = TRUE
)
