rm(list = ls())

setwd("C:/Users/Preet/OneDrive - Ursinus College/paid_labor/articleharvestr")

devtools::load_all()
devtools::document()

# scrape articles
sitemap_url <- "https://www.huffpost.com/sitemaps/sitemap-v1.xml"

article_urls <- gu_fetch_sitemap_articles(sitemap_url,
                                          levels = 1,
                                          start_date = "2024-01-01",
                                          end_date = "2025-03-25")

article_urls2 <- gu_remove_duplicates(article_urls)

# store article urls
sd_store_articles(
  article_data = article_urls2,
  news_site = "huffpost",
  overwrite = FALSE
)

# pull urls (or just continue from the last step)
test_data <- sd_pull_articles(
  start_date = "2024-01-01",
  end_date = "2025-03-25",
  news_site = "huffpost"
)

scraped_data <- sa_scrape_articles(test_data)