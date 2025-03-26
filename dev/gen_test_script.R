rm(list = ls())

setwd("C:/Users/Preet/OneDrive - Ursinus College/paid_labor/articleharvestr")

devtools::load_all()
devtools::document()

# scrape articles
# DONT NEED THIS FOR HUFFPOST ANYMORE... probably...
# ALL ARTICLE URLS /SHOULD/ BE IN inst/extdata/huffpost/index.json
# update ur dev mode packages yall
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

# more example code, also not using this rn
# pull urls (or just continue from the last step)
test_data <- sd_pull_articles(
  start_date = "2024-01-01",
  end_date = "2024-02-01",
  news_site = "huffpost",
  url = TRUE
)
View(test_data)

scraped_data <- sa_scrape_articles(test_data)
View(scraped_data)

sd_store_articles(
  article_data = scraped_data,
  news_site = "huffpost",
  overwrite = FALSE
)


# USE DIS PPL
# take random sample of urls, scrape them, and analyze them
# not doing random sample of scraped urls for now
# since what articles are scraped or not is pretty random
# ik its annoying to scrape every time but eh sorry
sampled_urls <- sd_sample_urls(
  start_date = "2023-01-01", # random aah date range,
  end_date = "2023-03-31",
  news_site = "huffpost",
  number = 100,
  period = "month"
)

scraped_data <- sa_scrape_articles(sampled_urls)
View(scraped_data)

sentiment_data <- as_article_sentiment(scraped_data)

# now u can store random sample of sentiment data
# or analyze it or make plots, whatever u want
